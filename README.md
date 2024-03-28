# Lexer para Expresiones Aritméticas

Este programa en Haskell funciona como un lexer para reconocer tokens en expresiones aritméticas. Lee expresiones de un archivo de texto y genera una tabla que contiene cada token reconocido junto con su tipo.

## Uso

1. Asegúrate de tener GHC (Glasgow Haskell Compiler) instalado en tu sistema.

2. Clona este repositorio o descarga el archivo `analizador.hs`.

3. Prepara un archivo de texto que contenga una o más expresiones aritméticas, cada una en una línea separada. Asegúrate de seguir las reglas especificadas en el enunciado del problema.

4. Ejecuta el programa con el siguiente comando en tu terminal:
    ```
    ghc analizador.hs
    ./analizador
    ```

5. Ingresa el nombre del archivo de texto que contiene las expresiones aritméticas cuando se te solicite.

6. El programa procesará las expresiones, reconocerá los tokens y los mostrará junto con sus tipos.

## Tipos de Tokens

Los tipos de tokens reconocidos son los siguientes:
- Variables: Deben comenzar con una letra (mayúscula o minúscula) y pueden contener letras, números y guiones bajos.
- Enteros: Números enteros.
- Flotantes (Reales): Números decimales, posiblemente en notación exponencial.
- Operadores: Asignación, Suma, Resta, Multiplicación, División y Potencia.
- Símbolos Especiales: Paréntesis para agrupación.
- Comentarios: Líneas que comienzan con "//" se tratan como comentarios.

## Implementación

- El lexer está implementado utilizando un autómata finito determinista (AFD) para reconocer y clasificar tokens según reglas predefinidas.

- Las transiciones entre estados del AFD se determinan analizando los caracteres de entrada de las expresiones.

## Autor

Angela Estefania Aguilar Medina
## Licencia

Este proyecto está bajo la [Licencia MIT](LICENSE).
