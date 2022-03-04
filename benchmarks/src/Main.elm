module Main exposing (main)

import Benchmark exposing (describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkTextIndex


main : BenchmarkProgram
main =
    program <|
        describe "Clad UI benchmarks"
            [ BenchmarkTextIndex.suite
            ]
