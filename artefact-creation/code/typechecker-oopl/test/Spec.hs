import Initial.Typechecker as Init
import Reader.Typechecker as Reader
import Backtrace.Typechecker as Back
import MultiError.Typechecker as Multi
import Warning.Typechecker as Warning

main :: IO ()
main = do
  putStrLn ""
  putStrLn "******************************************"
  putStrLn "**                                      **"
  putStrLn "** Simple compile test suite            **"
  putStrLn "**                                      **"
  putStrLn "******************************************"
  putStrLn ""
  putStrLn $ "Welcome to the test suite.\n" ++
             "The type checker is going to run some tests and show\n" ++
             "the output according to new features added to the compiler.\n"
  putStrLn $ "All versions of the type checker will type check the following\n"
              ++ "program, which contains 3 errors:\n"
              ++ "1. The class ``Foo'' is not defined\n"
              ++ "2. The class ``Bar'' is not defined\n"
              ++ "3. The class variable ``x'' is not defined"
  putStrLn ""
  putStrLn $ "class C\n" ++
             "  val f: Foo\n\n"
  putStrLn $ "class D\n" ++
             "  val g: Bar\n" ++
             "  def m(): Int\n    x\n\n"
  Init.testSuite
  Reader.testSuite
  Back.testSuite
  Warning.testSuite
  Multi.testSuite
  putStrLn ""
  putStrLn "Please scroll to the top and observe how each type checking iteration"
  putStrLn "adds a new feature. Thanks.\n"
