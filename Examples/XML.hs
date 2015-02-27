{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             FlexibleContexts, NoMonomorphismRestriction #-}

module Examples.XML where

{- |
This file contains some XML transformation examples
from XML Query Use Cases. 
-}
import Data.ApplicativeBX

import Data.Traversable (Traversable)
import Data.Functor
import Data.Foldable (Foldable)
import qualified Data.Foldable as F 

import Data.List 
import Data.Function (on)

import Control.Monad
import Control.Monad.List
import qualified Control.Monad.Trans as M

import Text.PrettyPrint.HughesPJ


data Tree a = N a [Tree a] -- polymorphic tree type
            deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

liftEq :: (Traversable t, Eq (t ())) =>
          (a -> a -> Bool) -> t (L s a) -> t (L s a) -> R s Bool
liftEq eq t1 t2 =
  if void t1 == void t2 then
    F.foldrM g True $ zip (F.toList t1) (F.toList t2)
  else
    return False
  where
    g (a1,a2) b = if b then liftO2 eq a1 a2 else return False

type BFilter s a = Tree a -> ListT (R s) (Tree a)

label (N x _) = x

data Lab = A String -- Attribute
         | T String -- Text
         | E String -- Element
       deriving (Show, Eq, Ord)

pretty (N (T t) []) = text t
pretty (N (E t) ts) = cat [text "<" <> text t <+>
                              hsep (map pretty attrs) <> text ">",
                           nest 4 $ cat (map pretty others),
                           text "</" <> text t <> text ">"]
    where
      (attrs,others) = partition p ts
          where p (N (A _) _) = True
                p _           = False
pretty (N (A a) [N (T t) []]) = text a <> text "=" <> text (show t)


-- from XML Query Use Cases, 1.1.2 Sample Data 
-- http://www.w3.org/TR/xquery-use-cases/#xmp-data
test_src
    = bib [
       book "1994" [
                 title [t "TCP/IP Illustrated"], 
                 author [last [t "Stevens"], first [t "W."]], 
                 publisher [t "Addison-Wesley"],
                 price     [t "65.95"]
                ], 
       book "1992" [
                 title [t "Advanced Programming in the Unix environment"], 
                 author [last [t "Stevens"], first [t "W."]], 
                 publisher [t "Addison-Wesley"],
                 price     [t "65.95"]
                ], 
       book "2000" [
                 title [t "Data on the Web"], 
                 author [last [t "Abiteboul"], first [t "Serge"]], 
                 author [last [t "Buneman"], first [t "Peter"]],
                 author [last [t "Suciu"], first [t "Dan"]],
                 publisher [t "Morgan Kaufmann Publishers"],
                 price     [t "39.95"]
                ],
       book "1999" [
                 title [t "The Economics of Technology and Content for Digital TV"],
                 editor [last [t "Gerbarg"], first [t "Darcy"], affil [t "CITI"]],
                 publisher [t "Kluwer Academic Publishers"],
                 price [t "129.95"]                              
                ]
      ]
    where 
      t x       = N (T x) []
      bib       = N (E "bib") 
      book y x  = N (E "book") ([N (A "year") [N (T y) []]]++x)
      title     = N (E "title")
      author    = N (E "author")
      publisher = N (E "publisher")
      price     = N (E "price")
      last      = N (E "last")
      first     = N (E "first")
      affil     = N (E "affiliation") 
      editor    = N (E "editor")

ofLabel :: Eq c => L s c -> BFilter s (L s c)
ofLabel l t = do { b <- M.lift $ liftO2 (==) l (label t)
                 ; if b then return t else mzero }

labelWith :: (c -> Bool) -> BFilter s (L s c)
labelWith p t = do { b <- M.lift $ liftO p (label t)
                   ; if b then return t else mzero }

children :: BFilter s a
children (N e ts) = ListT $ return ts 

childrenWith f = children >=> f 

union :: BFilter s a -> BFilter s a -> BFilter s a
union f1 f2 t = f1 t `mplus` f2 t 

deepest f (t@(N l ts)) = do { ck <- gather $ f t 
                            ; case ck of 
                                [] -> msum $ map (deep f) ts 
                                _  -> do { let rs = map (deep f) ts 
                                         ; msum $ (return t):rs }}


deep f t = bfs [t] [] 
    where
      bfs [] [] = mzero 
      bfs [] qs = bfs (reverse qs) [] 
      bfs (t@(N l ts):rest) qs = do { ck <- gather $ f t 
                                    ; case ck of 
                                        [] -> bfs rest (reverse ts ++ qs)
                                        _  -> mplus (return t) (bfs rest qs) }
                                              

multi f t = bfs [t] [] 
    where
      bfs [] [] = mzero 
      bfs [] qs = bfs (reverse qs) [] 
      bfs (t@(N l ts):rest) qs = do { ck <- gather $ f t 
                                    ; case ck of 
                                        [] -> bfs rest (reverse ts ++ qs)
                                        _  -> mplus (return t) (bfs rest (reverse ts ++ qs)) }

    
gather :: Monad m => ListT m a -> ListT m [a]
gather (ListT x) = 
    ListT $ do { a <- x 
               ; return $ [a] }

pick :: Monad m => ListT m a -> m a 
pick (ListT x) = do { a <- x
                    ; return $ head a }

isOk :: BFilter s a -> (Tree a -> ListT (R s) Bool)
isOk f t = do { ck <- gather $ f t 
              ; return $ not (null ck) }

guardM :: MonadPlus m => m Bool -> m ()        
guardM x = x >>= guard 

attr :: String -> L s Lab
attr = new . A 
el :: String -> L s Lab
el   = new . E
txt :: String -> L s Lab
txt  = new . T


f /> g = f >=> children >=> g 

keep :: BFilter s a
keep = return 

childrenOfLabel l = childrenWith (ofLabel l) 

-- Q1 
q1 t = pick $ 
       do { bs <- gather $ (keep /> (ofLabel (new $ E "book") >=> h)) t
          ; return $ N (new $ E "bib") bs }
    where
      h b = do { y  <- (keep /> ofLabel (new $ A "year") /> keep) b
               ; t  <- (keep /> ofLabel (new $ E "title")) b 
               ; p  <- (keep /> ofLabel (new $ E "publisher") /> keep) b
               ; guardM $ M.lift $ liftO2 ((>) `on` g) (label y) (new $ T "1991")
               ; guardM $ M.lift $ liftO2 (==) (label p) (new $ T "Addison-Wesley")
               ; return $ N (new $ E "book") [N (new $ A "year") [y], t] } 
          where g (T t) = read t :: Int  

q1L = unliftMT (fmap sequenceL . q1)

test_view_q1 
    = N (E "bib") [N (E "book") [N (A "year") [N (T "1994") []],
                                 N (E "title") [N (T "TCP/IP Illustrated (Second Edition)") []]],
                   N (E "book") [N (A "year") [N (T "1992") []],
                                 N (E "title") [N (T "Advanced Programming in the Unix Environment") []]]]

test_view_q1' 
    = N (E "bib") [N (E "book") [N (A "year") [N (T "1994") []],
                                 N (E "title") [N (T "TCP/IP illustrated") []]],
                   N (E "book") [N (A "year") [N (T "1991") []],
                                 N (E "title") [N (T "Advanced Programming in the Unix Environment") []]]]

{-
*Examples.XML> pretty test_src
<bib>
    <book year="1994">
        <title>TCP/IP Illustrated</title>
        <author><last>Stevens</last><first>W.</first></author>
        <publisher>Addison-Wesley</publisher>
        <price>65.95</price>
    </book>
    <book year="1992">
        <title>Advanced Programming in the Unix environment</title>
        <author><last>Stevens</last><first>W.</first></author>
        <publisher>Addison-Wesley</publisher>
        <price>65.95</price>
    </book>
    <book year="2000">
        <title>Data on the Web</title>
        <author><last>Abiteboul</last><first>Serge</first></author>
        <author><last>Buneman</last><first>Peter</first></author>
        <author><last>Suciu</last><first>Dan</first></author>
        <publisher>Morgan Kaufmann Publishers</publisher>
        <price>39.95</price>
    </book>
    <book year="1999">
        <title>
            The Economics of Technology and Content for Digital TV
        </title>
        <editor>
            <last>Gerbarg</last>
            <first>Darcy</first>
            <affiliation>CITI</affiliation>
        </editor>
        <publisher>Kluwer Academic Publishers</publisher>
        <price>129.95</price>
    </book>
</bib>
*Examples.XML> pretty $ get q1L test_src
<bib>
    <book year="1994"><title>TCP/IP Illustrated</title></book>
    <book year="1992">
        <title>Advanced Programming in the Unix environment</title>
    </book>
</bib>
*Examples.XML> pretty $ test_view_q1
<bib>
    <book year="1994">
        <title>TCP/IP Illustrated (Second Edition)</title>
    </book>
    <book year="1992">
        <title>Advanced Programming in the Unix Environment</title>
    </book>
</bib>
*Examples.XML> pretty $ put q1L test_src test_view_q1
<bib>
    <book year="1994">
        <title>TCP/IP Illustrated (Second Edition)</title>
        <author><last>Stevens</last><first>W.</first></author>
        <publisher>Addison-Wesley</publisher>
        <price>65.95</price>
    </book>
    <book year="1992">
        <title>Advanced Programming in the Unix Environment</title>
        <author><last>Stevens</last><first>W.</first></author>
        <publisher>Addison-Wesley</publisher>
        <price>65.95</price>
    </book>
    <book year="2000">
        <title>Data on the Web</title>
        <author><last>Abiteboul</last><first>Serge</first></author>
        <author><last>Buneman</last><first>Peter</first></author>
        <author><last>Suciu</last><first>Dan</first></author>
        <publisher>Morgan Kaufmann Publishers</publisher>
        <price>39.95</price>
    </book>
    <book year="1999">
        <title>
            The Economics of Technology and Content for Digital TV
        </title>
        <editor>
            <last>Gerbarg</last>
            <first>Darcy</first>
            <affiliation>CITI</affiliation>
        </editor>
        <publisher>Kluwer Academic Publishers</publisher>
        <price>129.95</price>
    </book>
</bib>
-}
{-
*Examples.XML> pretty $ test_view_q1'
<bib>
    <book year="1994"><title>TCP/IP illustrated</title></book>
    <book year="1991">
        <title>Advanced Programming in the Unix Environment</title>
    </book>
</bib>
*Examples.XML> pretty $ put q1L test_src test_view_q1'
*** Exception: Changing Observation
-}


