--Imports:
import Week40Exercise2

--Exercise 2:
toBinarySearchTree:: [a]-> BinSearchTree a 
toBinarySearchTree [] = Empty
toBinarySearchTree list = Branch (toBinarySearchTree firstHalf) node (toBinarySearchTree secondHalf) 
                          where
                            size = (length list)`div`2
                            firstHalf = take size list
                            secondHalf = drop (size+1) list
                            node = head $ drop size list
                            