module NRBF.Debug where

import Debug.Trace

debug_b = True

trace' arg = trace (show arg) arg

trace'str str arg = 
    case (debug_b) of
        True    -> trace ((str) ++ (show arg)) arg
        False   -> arg