{-# LANGUAGE OverloadedRecordDot      #-}
module RecordDot where

data P = P {x :: Float, y :: Float}

e p y = sqrt(p.x*p.x + y*y)

f p = e p p.y
