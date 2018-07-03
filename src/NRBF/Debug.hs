module NRBF.Debug where

import Debug.Trace
trace' arg = trace (show arg) arg

trace'str str arg = trace ((str) ++ (show arg)) arg