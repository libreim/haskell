-- Esto es un comentario

powerLevel = 9001

over9000 = powerLevel > 9000

hola  = "Hola"
adios = "Adiós"

holaMundo = (if over9000 then hola else adios) <> ", 🌍 !"

main = putStrLn holaMundo
