module Main where
import Hello (hello)
import Foo.Bar

main :: IO ()
main = hello "World"
