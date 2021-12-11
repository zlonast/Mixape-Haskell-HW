{-# LANGUAGE DerivingStrategies #-}
module HW1.Unit1.Day (Day(..), nextDay, afterDays, isWeekend, daysToParty) where

data Day = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday deriving stock Show

instance Enum Day where
  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday
  toEnum n = toEnum (n `mod` 7)

nextDay :: Day -> Day
nextDay = succ

afterDays :: Day -> Int -> Day
afterDays day num = toEnum (fromEnum day + num)

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: Day -> Int
daysToParty day = (fromEnum Friday - fromEnum day + numbersDay) `mod` 7
  where
    numbersDay :: Int
    numbersDay = 7

instance Eq Day where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  tThursday == Thursday  = True
  Friday    == Friday    = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False
