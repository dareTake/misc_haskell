import qualified Data.Map as M
import Data.Maybe(fromMaybe)

--definition for DFA
type State = String
type Symbol = Char
data DFA = DFA { 
            states :: [State]
            , alphabets :: [Symbol]
            , lookUpTable :: M.Map State (M.Map Symbol State) -- a map of map , eg: [ ("q0", [ ('a',"q1"), ('b',"q10") ]) ]
            , start :: State
            , acpStates :: [State]  
            } deriving (Show)

-- the transition function
transFn :: DFA -> State -> Symbol -> State
transFn dfa cur sym = nextState 
                        where
                            nextState = fromMaybe cur $ M.lookup sym transChart
                            transChart = lookUpTable dfa M.! cur

-- create an example DFA, 1st ex of Sipser's Book
createDFA :: String -> DFA
createDFA "sipser" = DFA allStates alphs table startState acceptStates
                    where
                        -- the lookup table consists of all the transition rules
                        allStates = ["q1", "q2", "q3"]
                        alphs = "01"
                        startState = "q1"
                        acceptStates = ["q2"]
                        q1_trans = M.fromList [('1', "q2"), ('0', "q1")]
                        q2_trans = M.fromList [('1', "q2"), ('0', "q3")]
                        q3_trans = M.fromList [('0', "q2"), ('1', "q2")]
                        q_trans = [q1_trans, q2_trans, q3_trans]
                        table = M.fromList $ zip allStates q_trans

-- does the string get acepted?
eval :: DFA -> String -> Bool
eval dfa str = foldl (transFn dfa) (start dfa) str `elem` acpStates dfa  

-- main IO
main :: IO ()
main = do
        putStrLn "Enter the string :"
        str <- getLine
        let dfa = createDFA "sipser" 
            res = eval dfa str
        putStrLn $ if res  then "Accepted" else "Rejected"

