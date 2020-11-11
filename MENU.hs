-------------------------------------------------------------
--Comienza el menú principal
main = do
    putStrLn("**********************************")
    putStrLn("*       --> BIENVENIDO <--       *")
    putStrLn("* -->AL MENÚ DE RECURSIVIDAD<--  *")
    putStrLn("**********************************")
    putStrLn("*    -->ELIGE UNA OPCIÓN<--      *")
    putStrLn("**********************************")
    putStrLn("* 1--> Serie Fibonacci           *")    
    putStrLn("* 2--> Números del 1-10          *") 
    putStrLn("* 3--> Mostrar Factorial         *") 
    putStrLn("* 4--> Desaparece Números        *") 
    putStrLn("* 5--> Palindromos               *") 
    putStrLn("* 6--> Calculadora               *") 
    putStrLn("* 7--> Bye Bye!!                 *") 
    putStrLn("**********************************")
    opcion <- getLine
    menu (read opcion)
---------------------------------------------------------
--Comienzan las opciones
menu opcion = do
    case opcion of 
        1 -> fibonacci -- terminado
        2 -> numeros 1 -- terminado
        3 -> numfact -- termindo
        4 -> desaparece [0,1,2,3,4,5,6,7,8,9,10] -- terminado
        5 -> palindromos -- terminado
        6 -> calculadora -- terminado
        7 -> salir    
        _ -> print("Opcion Invalida")    
-----------------------------------------------------------
--Opción 1

fibonacci = do
    putStrLn("Ingresa Una Posicion: ")
    posicion <- getLine
    
    print(fibo (read posicion))
    main
    
fibo posicion = do
    if posicion == 0
        then do
            0
    else if posicion == 1
        then do 
            1
    else
        fibo(posicion-1)+fibo(posicion-2)       
-------------------------------------------------------------
--Opción 2
numeros num = do
    if num <= 10
        then do
            print num
            numeros(num+1)
    else do
        putStrLn("termina")
        main
-------------------------------------------------------------
--Opción 3
numfact = do
    putStrLn("Ingresa El Número Para Factorial: ")
    fac <- getLine
    factorial (1)(read fac)(1)

factorial x y aux = do
    
    if x <= y
        then do
            print x
            factorial(x+1)(y)(aux*x)
    else
        putStrLn("termina")
    putStrLn("Factorial es: "++show(aux))
    main
--------------------------------------------------------------
--Opción 4
desaparece lista = do
    if null lista
        then do
            putStrLn("Termina")
            main
    else do
        print(lista)
        let lista2 = init lista
        desaparece(lista2)
---------------------------------------------------------------
--Opción 5
palindromos = do
    putStrLn("Ingresa el palindromo")
    pali <- getLine
    palindro (pali)
    
palindro pali = do
     if pali == reverse pali
        then do
            putStrLn("Si Es Un Palindromo")
            main
        else do
            putStrLn("No Es Un Palindromo")
            main
----------------------------------------------------------------
--Opción 6
calculadora = do
    putStrLn("1.- Suma")
    putStrLn("2.- Resta")
    putStrLn("3.- Multiplicacion")
    putStrLn("4.- Division")
    putStrLn("5.- Salir")
    putStrLn("Selecciona una opcion")
    n <- getLine
    caso (read n)
----------------------------------------------------------------
--Comienzan las opciones de la calculadora
caso n = do
    case n of
        1 -> suma
        2 -> resta
        3 -> multi
        4 -> division
        5 -> print("Salir")
        _ -> print("Opcion no valida")
        
suma = do
    putStrLn("Ingresa Un Número:")
    a <- getLine
    let c = read a::Int
    putStrLn("Ingresa Otro Número:")
    b <- getLine
    let d = read b::Int
    let resultado = c+d
    putStrLn("El resultado es: "++show resultado)
    main

resta = do
    putStrLn("Ingresa Un Número:")
    a <- getLine
    let c = read a::Int
    putStrLn("Ingresa Otro Número:")
    b <- getLine
    let d = read b::Int
    let resultado = c-d
    putStrLn("El resultado es: "++show(resultado))
    main
    
multi = do
    putStrLn("Ingresa Un Número:")
    a <- getLine
    let c = read a::Int
    putStrLn("Ingresa Otro Número:")
    b <- getLine
    let d = read b::Int
    let resultado = c*d
    putStrLn("El resultado es: "++show(resultado))
    main
    
division = do
    putStrLn("Ingresa Un Número:")
    a <- getLine
    let c = read a::Float
    putStrLn("Ingresa Otro Número:")
    b <- getLine
    let d = read b::Float
    let resultado = c/d
    putStrLn("El resultado es: "++show(resultado))
    main
-----------------------------------------------------------
--Opción 7
salir = do
    putStrLn("**********************************")
    putStrLn("*       --> BYE BYE!!! <--       *")
    putStrLn("*       -->ESTÁS FUERA <--       *")
    putStrLn("**********************************")
    main
