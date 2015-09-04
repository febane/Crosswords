{-
Crosswords
*****************************
@author Fernando Barbosa Neto
@author Eric Marchetti Santos
-}


import Data.List
import Data.Maybe


--teste 1

cpalavras = ['F','L','O','R','E','S','T','A','R','S', 
 'K','J','G','D','F','T','J','J','U','V',
 'C','A','P','E','L','A','P','I','S','C',
 'D','T','C','A','D','W','Y','E','R','T',
 'F','D','C','C','S','A','L','U','S','E',
 'Q','L','K','C','A','U','E','A','I','O',
 'A','A','S','A','R','P','R','D','K','A',
 'N','E','N','E','F','T','E','B','A','U',
 'J','K','D','D','S','H','W','L','F','L',
 'I','O','T','U','G','J','J','V','A','A']

lcustos = [('A',1),('B',10),('C',18),('D',38),('E',70),('F',10),('G',43),('H',15),('I',54),('J',10), ('K',44),('L',81),('M',19),('N',39),('O',54),('P',91),('Q',72),('R',30),('S',84),('T',75),('U',22),('V',47),('X',66),('Y',59),('Z',32)]

palavras = ["AULA", "ALEGRIA", "CAPELA", "FLORESTA", "LAPIS"]


--teste 2

testcpal = ['O','I','I','A']

testcl = [('O',1),('I',2),('A',3)]

testpal = ["OI","AI","OA"]


--Calcula a quantidade de linhas ou colunas da matriz
side x = round (sqrt (fromIntegral(length x)))

--Auxiliar de tomatrix
tomatrixaux cpal aside = if length cpal == aside
		      then [cpal]
		      else [take aside cpal] ++ tomatrixaux (drop aside cpal) aside

--Transforma uma lista em matriz
tomatrix cpal = if length cpal == (side cpal)
		then [cpal]
		else [take (side cpal) cpal] ++ tomatrixaux (drop (side cpal) cpal) (side cpal)
		
--Procura palavra da esquerda para a direita
findlr cpal pal = [x|x<-pal, i<-[0..((side cpal)-1)], isInfixOf x ((tomatrix cpal)!!i)]

--Procura palavra da direita para a esquerda
findrl cpal pal = [x|x<-pal, i<-[0..((side cpal)-1)], isInfixOf (reverse x) ((tomatrix cpal)!!i)]

--Procura palavra de cima para baixo
findud cpal pal = [x|x<-pal, i<-[0..((side cpal)-1)], isInfixOf x ((transpose (tomatrix cpal))!!i)]

--Procura palavra de baixo para cima
finddu cpal pal = [x|x<-pal, i<-[0..((side cpal)-1)], isInfixOf (reverse x) ((transpose(tomatrix cpal))!!i)]
--Procura (palavra, posiçao, direçao) da esquerda para a direita
foundlrpos cpal pal =  [(x, i, "horizontal-esquerda-direita")|x<-pal, i<-[0..((side cpal)-1)], isInfixOf x ((tomatrix cpal)!!i)]

--Procura (palavra, posiçao, direçao) da direita para a esquerda
findrlpos cpal pal = [(x, i, "horizontal-direita-esquerda")|x<-pal, i<-[0..((side cpal)-1)], isInfixOf (reverse x) ((tomatrix cpal)!!i)]

--Procura (palavra, posiçao, direçao) de cima para baixo
findudpos cpal pal = [(x, i, "vertical-topo-base")|x<-pal, i<-[0..((side cpal)-1)], isInfixOf x ((transpose (tomatrix cpal))!!i)]

--Procura (palavra, posiçao, direçao) de baixo para cima
finddupos cpal pal = [(x, i, "vertical-base-topo")|x<-pal, i<-[0..((side cpal)-1)], isInfixOf (reverse x) ((transpose(tomatrix cpal))!!i)]

--Retorna palavras encontradas no quadro de letras
found cpal pal = findlr cpal pal ++ findrl cpal pal ++ findud cpal pal ++ finddu cpal pal

--Calcula o custo da palavra
wordcost word cl cpal = if found cpal [word] == []
			then -1
			else sum [fromJust(lookup p cl) | p <- word]

--Cria lista dos custos das palavras
costs pal cpal cl = [wordcost word cl cpal|word<-pal]

ocorrenciamc pal cpal n cl = if foundlist == []
			     then error "Nenhuma palavra encontrada na matriz!"
			     else (fst rslt,snd rslt)
			     where rslt = (foundlist!!(fromJust(indexmini)),costlist!!(fromJust(indexmini)))
			   	   foundlist = found cpal pal
			   	   costlist = costs foundlist cpal cl
			   	   indexmini = elemIndex (minimum costlist) costlist

nocorrencias pal cpal n = sort [(x,length(found cpal [x]))|x<-pal, found cpal [x] /= []]

palavrasmaiscaras pal cpal n cl = reverse (sort [(costlist!!i,foundlist!!i)|i<-[0..((length foundlist) - 1)]])
			   	  where foundlist = found cpal pal
			   	 	costlist = costs foundlist cpal cl

dadospalavra pal cpal n = foundlrpos cpal pal ++ findrlpos cpal pal ++ findudpos cpal pal ++ finddupos cpal pal

direcao pal cpal n = if length (findlr cpal pal ++ findrl cpal pal) > length (findud cpal pal ++ finddu cpal pal)
		   then ("horizontal",length (findlr cpal pal ++ findrl cpal pal))
		   else if length (findlr cpal pal ++ findrl cpal pal) == length (findud cpal pal ++ finddu cpal pal)
		   	then ("ambas as direcoes",length (findlr cpal pal ++ findrl cpal pal))
		   	else ("vertical",length (findud cpal pal ++ finddu cpal pal))

saida = writeFile "saida.txt"  text

text = "Conjunto de palavras: CAPELA, LAPIS, FLOR, FLORESTA, AULA, ALEGRIA"  ++ "\n \nMatriz: n = " ++ show (side cpalavras) ++ "\n \n" ++ "FLORESTARS" ++ "\n" ++ "KJGDFTJJUV" ++ "\n" ++  "CAPELAPISC" ++ "\n" ++ "DTCADWYERT" ++ "\n" ++ "FDCCSALUSE" ++ "\n" ++ "QLKCAUEAIO" ++ "\n" ++ "AASARPRDKA" ++ "\n" ++ "NENEFTEBAU" ++ "\n" ++ "JKDDSHWLFL" ++ "\n" ++ "IOTUGJJVAA" ++ "\n" ++"\n" ++ show(ocorrenciamc palavras cpalavras (side cpalavras) lcustos) ++ "\n" ++ "\n" ++ show(nocorrencias palavras cpalavras (side cpalavras)) ++ "\n" ++ "\n" ++ show(palavrasmaiscaras palavras cpalavras (side cpalavras) lcustos) ++ "\n" ++ "\n" ++ show(dadospalavra palavras cpalavras (side cpalavras)) ++ "\n" ++ "\n" ++ show(direcao palavras cpalavras (side cpalavras))

