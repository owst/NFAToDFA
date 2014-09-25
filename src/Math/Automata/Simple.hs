{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Math.Automata.Simple
    ( NFA
    , DFA
    , flattenDFAStates
    , faToDot
    , textToNFA
    , subsetConstruction
    ) where

import Control.Arrow ( first )
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import Control.Lens ( use, _1, _2, (.=), (%=) )
import Control.Monad ( unless, void )
import Control.Monad.Trans.RWS ( evalRWS, tell )
import Control.Monad.Trans.State ( get, put, execState )
import Data.Foldable ( Foldable, foldr, for_, any )
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Hashable ( Hashable )
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NEL
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as T
import qualified Data.Text.Format as TF
import Text.Parsec ( oneOf, noneOf, many1, between, string, newline, option
                   , char, sepBy1, many, try, parse, eof, (<?>), lookAhead )
import Text.Parsec.Text.Lazy ( Parser )

import Prelude hiding ( foldr, any )

import qualified Data.HashSet.NonEmpty as NES
import qualified Data.HashMap.NonEmpty as NEHM

-- |Represents a finite automata. The main data structure is the transition
-- map, from states to lables to states. Abstract over DFA/NFA by taking
-- a Foldable parameter @f@ that determines the shape of the targets of
-- a given source/label Using NES means we have at least one initial state
-- and NEHM means that if we have an entry in the trans map, there is at
-- least one transition out of the src state.
data FA f s l where
    FA :: (Eq s, Eq l, Hashable s, Hashable l, Foldable f)
        => HashMap s (NEHM.NonEmpty l (f s)) -- ^ Transitions
        -> NES.NonEmpty s                    -- ^ Initial states
        -> HashSet s                         -- ^ Final states
        -> FA f s l

type NFA s l = FA NES.NonEmpty s l
type DFA s l = FA Identity s l

deriving instance (Show (f s), Show s, Show l) => Show (FA f s l)

-- |Render an FA to a graphviz DOT format graph.
faToDot :: forall f . (Foldable f) => FA f Text Text -> Text
faToDot (FA trans inits finals) =
    T.unlines [ "digraph NFA {"
              , "rankdir = LR;"
              , "node [shape = point]; init"
              -- Intercalate avoids an extra newline after transitions
              , T.intercalate "\n" stateTransLines
              , "}"
              ]
  where
    -- Processed source states, next state ID, state -> ID map
    initState = (HS.empty, (0 :: Integer, HM.empty))
    ((), stateTransLines) = evalRWS (for_ inits go) () initState

    twistedTrans = twistTrans trans

    -- We want to output a single transition for each (src,tgt) pair, so we
    -- need to "twist" the transition map into a map: s -> s -> NonEmptySet l
    -- We use the polymorphic type here, to make sure we don't make a mistake
    -- since instantiated at Text, the input/result maps have the same type!
    twistTrans :: (Eq s, Hashable s, Eq l, Hashable l)
               => HashMap s (NEHM.NonEmpty l (f s))
               -> HashMap s (NEHM.NonEmpty s (NES.NonEmpty l))
    twistTrans =
        -- Here, we don't know if (f s) will have any elements - if it
        -- doesn't, we don't want to add an entry in the new target -> lbls map
        HM.foldrWithKey srcMappingFolder HM.empty
      where
        srcMappingFolder src lbls =
             case foldLbls lbls of
                 Nothing -> id
                 Just nehm -> HM.insert src nehm

        foldLbls = NEHM.foldrWithKey lblFolder Nothing

        -- For each target, @t@, add a mapping t -> lbl
        lblFolder l ts m = foldr (insertNESUnion l) m ts

        -- If we already have a NEHM, union in the lbl/target mapping,
        -- otherwise construct a new NEHM
        insertNESUnion lbl tgt mbNEHM =
            let sLbl = NES.singleton lbl
            in Just $ case mbNEHM of
                Nothing -> NEHM.singleton tgt sLbl
                Just nehm -> NEHM.insertWith NES.union tgt sLbl nehm

    emitLine l = tell [l]

    go src = do
        processed <- haveProcessedSrc src
        unless processed $ do
            markSrcProcessed src
            let srcTrans = NEHM.toList <$> src `HM.lookup` twistedTrans
            case srcTrans of
                Nothing ->
                    -- If there are no transitions, simply emit the src state
                    void $ getOrGenerateStateID src
                Just tgtLbls -> for_ tgtLbls $ \(tgt, lbls) -> do
                    transLine src lbls tgt >>= emitLine
                    go tgt

    haveProcessedSrc s = (s `HS.member`) <$> use _1
    markSrcProcessed s = _1 %= (s `HS.insert`)

    getOrGenerateStateID s = do
        (nextID, nameMap) <- use _2
        case s `HM.lookup` nameMap of
            Just sid -> return sid
            -- A new state; emit it, and update the next id and id map
            Nothing -> do
                stateLine s nextID >>= emitLine
                _2 .= (succ nextID, HM.insert s nextID nameMap)
                return nextID

    stateLine s sid = do
        let -- If this state is initial, add an arrow from the init point to it
            initText = if s `NES.member` inits
                           then TF.format " init -> {};" (TF.Only sid)
                           else ""
            shape :: Text
            shape = if s `HS.member` finals then "doublecircle" else "circle"
            fmtString = "{} [label=\"{}\" shape=\"{}\" ];{}"
        return $ TF.format fmtString (sid, s, shape, initText)

    transLine s ls t = do
        sid <- getOrGenerateStateID s
        tid <- getOrGenerateStateID t
        let fmtString = "{} -> {} [label=\"{}\"];"
        return $ TF.format fmtString (sid, tid, showNES ls)

    -- If the NES has >1 item show it in braces, else show the item alone.
    showNES nes = case NES.toList nes of
                      [t] -> t
                      ts -> addBraces . T.intercalate ", " $ ts
      where
        addBraces to = T.concat ["{", to, "}"]

-- |Used to turn the states of a DFA produced by subsetConstruction into
-- a DFA that can be converted to DOT, by forming a textual representation
-- of the sets of NFA states.
flattenDFAStates :: DFA (NES.NonEmpty T.Text) T.Text -> DFA T.Text T.Text
flattenDFAStates (FA trans inits finals) =
    FA trans' (NES.map showNES inits) (HS.map showNES finals)
  where
    trans' = renameTargets . renameSources $ trans

    -- fmap inside the two HashMaps and the Identity
    renameTargets = fmap (fmap (fmap showNES))
    -- Yuck, but it seems it's the best we can do!
    renameSources = HM.fromList . fmap (first showNES) . HM.toList

    showNES :: NES.NonEmpty T.Text -> T.Text
    showNES nes = T.concat [ "{"
                           , T.intercalate "," $ NES.toList nes
                           , "}"
                           ]


-- subsetConstruction converts a NFA to a DFA, by consider sets of NFA
-- states to be a single DFA state.
subsetConstruction :: forall s l . (Show s, Show l) => NFA s l
                   -> DFA (NES.NonEmpty s) l
subsetConstruction (FA trans inits finals) = FA trans' inits' finals'
  where
    inits' :: NES.NonEmpty (NES.NonEmpty s)
    inits' = NES.singleton inits
    (trans', states') = execState (go inits) (HM.empty, inits')

    -- The new finals are any state set that contains an original final state
    finals' = HS.filter (any (`HS.member` finals)) $ NES.toHashSet states'

    -- |Lift transitions from single sources to sets of sources, unioning the
    -- underlying maps
    liftTrans :: NES.NonEmpty s -> Maybe (NEHM.NonEmpty l (NES.NonEmpty s))
    liftTrans s =
        case mapMaybe getTrans . NES.toList $ s of
            [] -> Nothing
            nehms -> Just $ foldr1 (NEHM.unionWith NES.union) nehms
      where
        getTrans :: s -> Maybe (NEHM.NonEmpty l (NES.NonEmpty s))
        getTrans src = src `HM.lookup` trans

    -- |Get the set of targets that the liftedTransitions map to.
    getTransTargets :: NEHM.NonEmpty l (NES.NonEmpty s) -> [NES.NonEmpty s]
    getTransTargets = map snd . NEHM.toList

    go s = do
        let transitions = liftTrans s
        case transitions of
            Nothing -> return ()
            Just trans -> do
                let newTransTargets = getTransTargets trans
                (transSoFar, done) <- get
                -- Which new states haven't we seen before?
                let todoStates = filter (not . (`NES.member` done)) newTransTargets
                    -- Each Set of NFA states is now a single set of the DFA,
                    -- furthermore, each label now maps to a single such state, as
                    -- indicated by Identity as the functor parameter of FA
                    singleTargets = fmap Identity trans
                    transSoFar' = HM.insert s singleTargets transSoFar
                    done' = foldr NES.insert done todoStates
                put (transSoFar', done')
                for_ todoStates go

-- |Parse a NFA from an input Text.
textToNFA :: Text -> Either String (NFA Text Text)
textToNFA = (nfaDefToNFA <$>) . doParse
  where
    doParse = showLeft . parse parseNFADef ""
    showLeft = either (Left . show) Right

-- |Convert a NFADef into a NFA. At this stage, duplicate states/transitions
-- are removed.
nfaDefToNFA :: NFADef -> NFA Text Text
nfaDefToNFA (NFADef is ts fs) =
    FA trans (NES.fromNonEmptyList is) (HS.fromList fs)
  where
    trans = HM.fromListWith (NEHM.unionWith NES.union) singleTrans
    -- ss and ls can be treated as non-empty lists; we use the list Monad to
    -- construct a single list of pairs from source states to a (singleton)
    -- HashMap from labels to targets
    singleTrans = do
        (NFATransDef srcs ls trgs) <- ts
        s <- NEL.toList srcs
        l <- NEL.toList ls
        return (s, NEHM.singleton l (NES.fromNonEmptyList trgs))

data NFADef = NFADef (NEL.NonEmpty Text) -- ^ Initial states
                     [NFATransDef]   -- ^ Transitions
                     [Text]          -- ^ Final states
            deriving Show

data NFATransDef = NFATransDef (NEL.NonEmpty Text) -- ^ Source states
                               (NEL.NonEmpty Text) -- ^ Labels
                               (NEL.NonEmpty Text) -- ^ Target states
                 deriving Show

-- Parse a NFA definition:
--  NFA ::= STATES, "\n", { TRANS, "\n" }, [ STATES "\n" ];
--
--  STATE ::= CHARS
--
--  STATES ::= STATE { ",", STATE };
--
--  TRANS ::= STATES, "--", LABELS, "->", STATES;
--
--  LABELS ::= LABEL { ",", LABELS };
--
--  LABEL ::= CHARS
--
--  CHARS ::= {- Unicode String that doesn't contain unescaped ',', '\', '>', '-' or '\n' -};
--
-- E.g.:
--     0
--     0--b,c->1
--     0--a->0,1
--     1
--
-- is the NFA that has two states and accepts any string a*(a|b|c).
parseNFADef :: Parser NFADef
parseNFADef =
    NFADef <$> (parseStates "initial" <* newline)
           <*> many (isTransLine *> parseNFATrans <* newline)
           <*> option []
                      (NEL.toList <$>
                          parseStates "final" <* option '\n' newline)
           <* eof
  where
    commaSep1 x = sepBy1 x (char ',')

    -- Since the initial portion of a trans line looks like a state, we use
    -- @try@ to backtrack out if we fail by not being on a trans line. We
    -- don't just use try, because if we fail inside the trans line (e.g.
    -- invalid escape) we don't want to backtrack and attempt to parse the
    -- final states.
    isTransLine = try . lookAhead $ parseStates "source" >> string "--"

    parseTextString c = T.pack <$> many1 c

    parseStates pType =
        toNELOrError (pType ++ " states") <$> commaSep1 parseState

    parseState = parseTextString parseChar

    -- Allow simple escape sequences:
    -- \\ -> \
    -- \- -> -
    -- \, -> ,
    -- \> -> >
    parseChar = do
        c <- noneOf "-,>\n"
        if c == '\\'
            then oneOf "\\-,>" <?> "valid escape char: '\\', or '-' or '>'"
            else return c

    parseNFATrans =
        NFATransDef <$> parseStates "source"
                    <*> between (string "--") (string "->") parseLabels
                    <*> parseStates "target"

    parseLabels = toNELOrError "labels" <$>
        (commaSep1 . parseTextString $ parseChar)

    toNELOrError elemType es = fromMaybe err (NEL.nonEmpty es)
      where
        err = error $ "Must have non-empty set of " ++ elemType
