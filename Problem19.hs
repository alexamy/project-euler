module Problem19 where

data DayOfWeek = Monday | Tuesday  | Wednesday | Thursday
               | Friday | Saturday | Sunday
               deriving (Eq, Ord, Show)

instance Enum DayOfWeek where
  toEnum n =
    case mod n 7 of
      0 -> Monday
      1 -> Tuesday
      2 -> Wednesday
      3 -> Thursday
      4 -> Friday
      5 -> Saturday
      6 -> Sunday
  fromEnum day =
    case day of
      Monday    -> 0
      Tuesday   -> 1
      Wednesday -> 2
      Thursday  -> 3
      Friday    -> 4
      Saturday  -> 5
      Sunday    -> 6

data Month = January    | February | March    | April
           | May        | June     | July     | August
           | September  | October  | November | December
           deriving (Eq, Ord, Show)

instance Enum Month where
  toEnum n =
    case mod n 12 of
      0  -> January
      1  -> February
      2  -> March
      3  -> April
      4  -> May
      5  -> June
      6  -> July
      7  -> August
      8  -> September
      9  -> October
      10 -> November
      11 -> December
  fromEnum month =
    case month of
      January    -> 0
      February   -> 1
      March      -> 2
      April      -> 3
      May        -> 4
      June       -> 5
      July       -> 6
      August     -> 7
      September  -> 8
      October    -> 9
      November   -> 10
      December   -> 11

type Year = Integer
type DayInMonth = Integer

data Date = Date Year Month DayInMonth
 deriving (Eq, Ord, Show)

fromEnum (Date year month day) =
  let monthDays = if month == January then 0 else
                  sum $ map (daysInMonth year) [m | m <- [January .. (pred month)]]
      yearDays  = sum $ map daysInYear [y | y <- [1990 .. year-1]]
  in day + monthDays + yearDays

daysInYear year =
  case isLeapYear year of
    True  -> 366
    False -> 365

daysInMonth :: Year -> Month  -> Integer
daysInMonth _ April = 30
daysInMonth _ June = 30
daysInMonth _ September = 30
daysInMonth _ November = 30

daysInMonth _ January = 31
daysInMonth _ March = 31
daysInMonth _ May = 31
daysInMonth _ July = 31
daysInMonth _ August = 31
daysInMonth _ October = 31
daysInMonth _ December = 31

daysInMonth year February =
  case isLeapYear year of
    False -> 28
    True  -> 29

isLeapYear :: Year -> Bool
isLeapYear year
  | isDividble year 400 = True
  | isDividble year 100 = False
  | otherwise = isDividble year 4
  where
    isDividble n md = (mod n md) == 0

dateToDayOfWeek :: Integer -> Month -> Year -> DayOfWeek
dateToDayOfWeek 1   January 1900 = Monday
--dateToDayOfWeek day month   year =
