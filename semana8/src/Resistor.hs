module Resistor where


-- color codes for resistors

data Color
  = Black | Brown
  | Red | Orange
  | Yellow | Green
  | Blue | Violet
  | Gray | Gold | Silver
  deriving (Eq, Ord)


-- Resistence table

type Table = [(Color, [Maybe Float])]

table :: Table
table = [(Black,  [Just 0.0, Just 0.0, Just 0.0]),
         (Brown,  [Just 1.0, Just 1.0, Just 10.0]),
         (Red,    [Just 2.0, Just 2.0, Just 100.0]),
         (Orange, [Just 3.0, Just 3.0, Just 1000.0]),
         (Yellow, [Just 4.0, Just 4.0, Just 10000.0]),
         (Green,  [Just 5.0, Just 5.0, Just 100000.0]),
         (Blue,   [Just 6.0, Just 6.0, Just 1000000.0]),
         (Violet, [Just 7.0, Just 7.0, Just 100000000.0]),
         (Gray,   [Just 8.0, Just 8.0, Just 1000000000.0]),
         (Gold,   [Nothing,  Nothing, Just 0.1]),
         (Silver, [Nothing,  Nothing, Just 0.01])]

-- exercise 1.

value :: Color -> Int -> Maybe Float
value color n 
  = if n < 0 || n > 3 then Nothing 
  else case lookup color table of
      Nothing         -> Nothing
      Just floats     -> (head (drop n floats))

newtype Resistor
  = Resistor [Color]
  deriving (Eq, Ord)

-- exercise 2.

valid :: Resistor -> Bool
valid (Resistor colors) = length colors == 3

-- exercise 3.

resistence :: Resistor -> Float
resistence (Resistor colors) = (multiply . fst . foldr step base) colors
  where
    multiply (x : xs) = (x * 10 + (head xs)) * head (tail xs)
    step color (xs, index) = 
      case value color index of
        Nothing   -> (xs, index - 1)
        Just v    -> (v : xs, index -1)
    base = ([], length colors - 1)
      
data Circuit = Parallel Circuit Circuit
             | Series   Circuit Circuit
             | Single Resistor
             deriving (Eq, Ord)

parallelCirc = Parallel 
                  (Single (Resistor [Red, Brown, Yellow])) 
                  (Single (Resistor [Red, Brown, Yellow]))
               

serieCirc = Series (Series 
                        (Single (Resistor [Black, Blue, Yellow])) 
                        (Single (Resistor [Red, Yellow, Blue])))
                  (Single (Resistor [Orange, Brown, Silver]))

validCircuit :: Circuit -> Bool
validCircuit (Parallel c1 c2) = validCircuit c1 && validCircuit c2
validCircuit (Series c1 c2) = validCircuit c1 && validCircuit c2
validCircuit (Single circuit) = valid circuit

circuitResistence :: Circuit -> Float
circuitResistence (Parallel c1 c2)  = 1 / (1 / (circuitResistence c1) + 1 / (circuitResistence c2))
circuitResistence (Series c1 c2)    = (circuitResistence c1 + circuitResistence c2)
circuitResistence (Single circuit)  = resistence circuit
