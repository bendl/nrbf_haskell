module NRBF.Debug where

import Debug.Trace
trace' arg = trace (show arg) arg

trace'str str arg = trace ((show str) ++ (show arg)) arg