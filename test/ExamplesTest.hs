module ExamplesTest where
import Test.HUnit (test, Test(TestLabel, TestCase), assertEqual, (~:), (~=?))
import Common (Solution(solutionRun, solutionName), NoSolution(..))
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


-- cases format is as follows:
-- (part1, part2)
-- where part1 and part2 are lists: [(expected1, in1), (expected2, in2), ...]
exampleTest solution (part1Cases, part2Cases) = let 
    name = solutionName solution
    makeTest part tuplePart (input, expected) = part ~: input ~: expected ~=? tuplePart (solutionRun solution input)
    in
    TestLabel name $ test [ 
        test $ map (makeTest "part1" fst) part1Cases,
        test $ map (makeTest "part2" snd) part2Cases ]

day01input = "1000\n" ++
             "2000\n" ++
             "3000\n" ++
             "\n" ++
             "4000\n" ++
             "\n" ++
             "5000\n" ++
             "6000\n" ++
             "\n" ++
             "7000\n" ++
             "8000\n" ++
             "9000\n" ++
             "\n" ++
             "10000\n"

day01part1 = [(day01input, 24000)]
day01part2 = [(day01input, 45000)]

day02part1 = []
day02part2 = []

day03part1 = []
day03part2 = []

day04part1 = []
day04part2 = []

day05part1 = []
day05part2 = []

day06part1 = []
day06part2 = []

day07part1 = []
day07part2 = []

day08part1 = []
day08part2 = []

day09part1 = []
day09part2 = []

day10part1 = []
day10part2 = []

day11part1 = []
day11part2 = []

day12part1 = []
day12part2 = []

day13part1 = []
day13part2 = []

day14part1 = []
day14part2 = []

day15part1 = []
day15part2 = []

day16part1 = []
day16part2 = []

day17part1 = []
day17part2 = []

day18part1 = []
day18part2 = []

day19part1 = []
day19part2 = []

day20part1 = []
day20part2 = []

day21part1 = []
day21part2 = []

day22part1 = []
day22part2 = []

day23part1 = []
day23part2 = []

day24part1 = []
day24part2 = []

day25part1 = []
day25part2 = []

tests = TestLabel "ExamplesTest" $ test
    [ exampleTest Day01.solution (day01part1, day01part2)
    , exampleTest Day02.solution (day02part1, day02part2)
    , exampleTest Day03.solution (day03part1, day03part2)
    , exampleTest Day04.solution (day04part1, day04part2)
    , exampleTest Day05.solution (day05part1, day05part2)
    , exampleTest Day06.solution (day06part1, day06part2)
    , exampleTest Day07.solution (day07part1, day07part2)
    , exampleTest Day08.solution (day08part1, day08part2)
    , exampleTest Day09.solution (day09part1, day09part2)
    , exampleTest Day10.solution (day10part1, day10part2)
    , exampleTest Day11.solution (day11part1, day11part2)
    , exampleTest Day12.solution (day12part1, day12part2)
    , exampleTest Day13.solution (day13part1, day13part2)
    , exampleTest Day14.solution (day14part1, day14part2)
    , exampleTest Day15.solution (day15part1, day15part2)
    , exampleTest Day16.solution (day16part1, day16part2)
    , exampleTest Day17.solution (day17part1, day17part2)
    , exampleTest Day18.solution (day18part1, day18part2)
    , exampleTest Day19.solution (day19part1, day19part2)
    , exampleTest Day20.solution (day20part1, day20part2)
    , exampleTest Day21.solution (day21part1, day21part2)
    , exampleTest Day22.solution (day22part1, day22part2)
    , exampleTest Day23.solution (day23part1, day23part2)
    , exampleTest Day24.solution (day24part1, day24part2)
    , exampleTest Day25.solution (day25part1, day25part2) ]
