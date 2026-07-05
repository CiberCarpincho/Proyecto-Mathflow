# MathFlow — Documentación Técnica

**Proyecto Final — Fundamentos de Lenguajes de Programación**
**Universidad del Valle**

## Integrantes

- Steven Fernando Aragón — 2418804
- Manuela Martínez Moncada — 2375458
- Andrés Gerardo González — 2416541

**Repositorio:** https://github.com/CiberCarpincho/Proyecto-Mathflow.git

Este documento especifica el diseño del lenguaje **MathFlow** para los puntos **2 (sintaxis gramatical general), 4 (listas), 5 (diccionarios) y 6 (control y recursión)**: especificación léxica, gramática, semántica de cada construcción, primitivas y ejemplos de invocación mediante `scan&parse`, tal como lo exige la sección 1 del enunciado ("La gramática deberá contener ejemplos de cada producción utilizando llamados a `scan&parse`").

---

## 1. Modelo de valores

MathFlow es un lenguaje de **tipado dinámico**: los identificadores no tienen un tipo fijo, el valor que almacenan en cada momento determina su comportamiento.

- **Valores denotados:** `Ref(valor-expresado)`. Todo identificador ligado por `var` o `const` se asocia a una referencia que contiene, además del valor, la marca de si es mutable (`var`) o inmutable (`const`).
- **Valores expresados:** enteros, flotantes, cadenas de caracteres, booleanos (`true`/`false`), `null`, funciones (closures), listas y diccionarios.
- **Mutabilidad:** números, cadenas, booleanos y funciones son inmutables como valores; las **listas y los diccionarios son estructuras mutables** — pueden modificarse en tiempo de ejecución sin crear un nuevo objeto.
- **Valor de verdad dinámico:** en cualquier contexto que requiera un booleano (condición de `if`, `while`, `and`/`or`/`not`), se consideran **falsos** `false`, `0`, `""` y `null`; cualquier otro valor se considera verdadero.

---

## 2. Especificación léxica

```
identifier ::= <letter> ( <letter> | <digit> )*
letter     ::= A..Z | a..z

number     ::= <digit>+
            |  "-" <digit>+
            |  <digit>+ "." <digit>+
            |  "-" <digit>+ "." <digit>+

string     ::= "\"" ( cualquier carácter distinto de \" )* "\""

comment    ::= "#" ( cualquier carácter distinto de salto de línea )*   ; se descarta, no genera token
whitespace ::= espacios, tabs, saltos de línea                         ; se descarta, no genera token
```

---

## 3. Punto 2 — Sintaxis gramatical general

### 3.1 Programa

```
<program> ::= <expression>
```

Un programa MathFlow es una única expresión (típicamente un bloque `begin ... end`). Todas las construcciones del lenguaje son expresiones y producen un valor.

### 3.2 Identificadores: lectura y actualización

```
<expression> ::= <identifier> <identifier-tail>

<identifier-tail> ::= ε                    ; lectura del valor actual
                   |  "=" <expression>      ; actualización (requiere que el identificador
                                            ;  haya sido declarado previamente con "var")
```

**Semántica:**
- La forma vacía busca el valor ligado al identificador en el ambiente y lo devuelve.
- La forma con `=` reevalúa la expresión de la derecha y reemplaza el valor asociado al identificador, **siempre que este haya sido declarado con `var`**. Intentar actualizar un identificador declarado con `const` produce un error en tiempo de ejecución; intentar actualizar un identificador no declarado también es un error.

**Ejemplos `scan&parse`:**
```racket
(scan&parse "x")            ; lectura
(scan&parse "x = 20")       ; actualización
```

### 3.3 Literales

```
<expression> ::= <number>
              |  <string>
              |  "true"
              |  "false"
              |  "null"
```

**Semántica:** cada literal se evalúa a sí mismo. `<number>` produce un entero o flotante según la regla léxica que haya emparejado. `<string>` produce la cadena sin las comillas que la delimitan. `null` representa la ausencia de valor.

**Ejemplos:**
```racket
(scan&parse "42")
(scan&parse "-3.5")
(scan&parse "\"hola mundo\"")
(scan&parse "true")
(scan&parse "false")
(scan&parse "null")
```

### 3.4 Declaración de variables y constantes

```
<expression>  ::= "var" "{" <declaraciones> "}"
              |  "const" "{" <declaraciones> "}"

<declaraciones>      ::= <identifier> "=" "(" <expression> ")" <declaraciones-tail>
<declaraciones-tail> ::= ε
                      |  "," <declaraciones>
```

**Semántica:** una definición `var` introduce una o más variables **actualizables**, con sus valores iniciales; una definición `const` introduce una o más **constantes**, no actualizables. Ambas admiten declarar cualquier cantidad de identificadores en una sola sentencia, separados por comas, y cada valor se evalúa en el ambiente vigente antes de ligarlo. Un mismo identificador puede cambiar de **tipo de valor** (entero, cadena, booleano, etc.) a lo largo de la ejecución si fue declarado con `var`, pero no puede cambiar su condición de mutable/inmutable.

**Ejemplos:**
```racket
(scan&parse "var { x = (42) }")
(scan&parse "const { y = (10) }")
(scan&parse "var { x1 = (1) , x2 = (2) , x3 = (3) }")
(scan&parse "const { a = (1) , b = (2) }")
```

### 3.5 Secuenciación

```
<expression> ::= "begin" <expression> { ";" <expression> }* "end"
```

**Semántica:** agrupa una o más expresiones en un bloque, evaluadas estrictamente en orden. El valor del bloque completo es el valor de la **última** expresión evaluada; toda ligadura o actualización hecha en una expresión del bloque es visible para las expresiones siguientes dentro del mismo bloque.

**Ejemplo:**
```racket
(scan&parse "begin var { x = (5) } ; var { y = (10) } ; print(+(x, y)) end")
```

### 3.6 Definición e invocación de funciones

```
<expression> ::= "func" <identifier> "(" <parametros> ")" "{" <cuerpo> "}"

<parametros> ::= ε
             |  <identifier> { "," <identifier> }*

<cuerpo> ::= <expression> { ";" <expression> }*

<expression> ::= "return" <expression>

<expression> ::= "(" <expression> { <expression> }* ")"    ; invocación / aplicación
```

**Semántica:**
- `func` liga el nombre de la función a un procedimiento (closure) que puede invocarse por su nombre, incluyendo **de forma recursiva** dentro de su propio cuerpo — la recursión está soportada por defecto, sin necesidad de una construcción adicional tipo `letrec`.
- El cuerpo de la función es una secuencia de expresiones (igual semántica que `begin`). Si el cuerpo no ejecuta ningún `return`, la función retorna `null` automáticamente.
- El paso de parámetros es **por valor** para enteros, flotantes, cadenas, booleanos y funciones, y **por referencia** para listas y diccionarios (dado que estas estructuras son mutables): una función puede modificar en sitio una lista o diccionario recibido como argumento, y el cambio es visible para quien la invocó.
- La invocación de una función se escribe encerrando el nombre de la función y sus argumentos entre paréntesis, sin comas entre ellos: `(nombre arg1 arg2 ...)`.

**Ejemplos:**
```racket
(scan&parse "func sumar (a, b) { return +(a, b) }")
(scan&parse "func saludar (nombre) { print(concatenar(\"Hola, \", nombre)) }")
(scan&parse "func factorial (n) { if <=(n, 1) then return 1 else return *(n, (factorial -(n, 1))) end }")
(scan&parse "(sumar 3 4)")
(scan&parse "(factorial 5)")
```

### 3.7 Primitivas aritméticas, relacionales y lógicas

Notación **prefija**, con el nombre de la primitiva seguido de sus operandos entre paréntesis separados por comas:

```
<primitiva-aritmetica>  ::= "+" | "-" | "*" | "/" | "%" | "add1" | "sub1"
<primitiva-relacional>  ::= "<" | ">" | "<=" | ">=" | "==" | "<>"
<primitiva-logica>      ::= "and" | "or" | "not"
```

**Semántica:** `+`, `-`, `*`, `/` y `%` operan sobre enteros y flotantes; `add1`/`sub1` suman/restan 1 a su único argumento. Las relacionales comparan dos operandos y producen un booleano. `and`/`or` son binarias y evalúan la verdad dinámica de sus dos argumentos; `not` es unaria.

**Ejemplos:**
```racket
(scan&parse "+(2, 3)")
(scan&parse "-(10, 4)")
(scan&parse "*(6, 7)")
(scan&parse "/(20, 4)")
(scan&parse "%(10, 3)")
(scan&parse "add1(5)")
(scan&parse "sub1(5)")
(scan&parse "<(a, b)")
(scan&parse ">=(a, b)")
(scan&parse "==(a, b)")
(scan&parse "<>(a, b)")
(scan&parse "and(true, false)")
(scan&parse "or(true, false)")
(scan&parse "not(true)")
```

### 3.8 Primitivas sobre cadenas

```
<primitiva-cadena> ::= "longitud" | "concatenar"
```

**Semántica:** `longitud(cadena)` devuelve la cantidad de caracteres; `concatenar(cadena1, cadena2)` devuelve la unión de ambas.

**Ejemplos:**
```racket
(scan&parse "longitud(\"MathFlow\")")
(scan&parse "concatenar(\"Hola, \", \"mundo\")")
```

### 3.9 Salida estándar

```
<primitiva-io> ::= "print"
```

**Semántica:** `print(exp)` evalúa `exp` y escribe su representación textual en salida estándar (números tal cual, cadenas sin comillas, booleanos como `true`/`false`, `null` como `null`, listas como `[e1, e2, ...]`, diccionarios como `{"clave": valor, ...}`); devuelve `null`.

**Ejemplo:**
```racket
(scan&parse "print(\"Hola, MathFlow\")")
```

---

## 4. Punto 4 — Listas

### 4.1 Representación interna

Una lista MathFlow no vacía se representa como una estructura mutable de tres campos: una etiqueta (`lista-mathflow`), su primer elemento (cabeza) y el resto de la lista (cola). La lista vacía es un valor constante distinguido, `vacio`. Esta representación corresponde a la nota semántica del enunciado:

```
crear-lista(x, xs) ≡ (x . xs)
```

La mutabilidad se logra porque los campos de la estructura pueden reemplazarse en sitio (usado por `set-list`), de modo que cualquier otra referencia a la misma lista ve el cambio — esto es lo que sustenta el paso por referencia de listas a funciones (sección 3.6).

### 4.2 Gramática

```
<expression>  ::= "vacio"
<primitiva-lista> ::= "crear-lista" | "vacio?" | "lista?" | "cabeza" | "cola"
                   |  "ref-list" | "append" | "set-list"
```

### 4.3 Primitivas

| Primitiva | Semántica |
|---|---|
| `vacio` | Constante que representa la lista vacía. |
| `vacio?(lst)` | `true` si `lst` es la lista vacía, `false` en caso contrario. |
| `crear-lista(elem, lst)` | Construye una nueva lista anteponiendo `elem` a `lst` (equivalente a `cons`). |
| `lista?(x)` | `true` si `x` es una lista MathFlow válida (vacía o no). |
| `cabeza(lst)` | Primer elemento de `lst`. Error si `lst` es vacía. |
| `cola(lst)` | Lista con todos los elementos de `lst` excepto el primero. Error si `lst` es vacía. |
| `append(lst1, lst2)` | Nueva lista resultado de concatenar `lst1` y `lst2`. |
| `ref-list(lst, i)` | Elemento en la posición `i` de `lst` (índices desde 0); `null` si el índice está fuera de rango. |
| `set-list(lst, i, valor)` | Reemplaza el elemento en la posición `i` de `lst` por `valor` **mutando la lista en sitio**; devuelve la lista modificada. |

### 4.4 Ejemplos `scan&parse` de cada producción

```racket
(scan&parse "vacio")
(scan&parse "crear-lista(3, vacio)")
(scan&parse "vacio?(vacio)")
(scan&parse "lista?(crear-lista(1, vacio))")
(scan&parse "cabeza(crear-lista(1, crear-lista(2, vacio)))")
(scan&parse "cola(crear-lista(1, crear-lista(2, vacio)))")
(scan&parse "append(crear-lista(1, vacio), crear-lista(2, vacio))")
(scan&parse "ref-list(crear-lista(1, crear-lista(2, vacio)), 1)")
(scan&parse "set-list(crear-lista(1, crear-lista(2, vacio)), 1, 99)")
```

### 4.5 Ejemplo de uso completo

```racket
(eval-program (scan&parse
  "begin
     var { lista = (crear-lista(3, vacio)) };
     lista = crear-lista(2, lista);
     lista = crear-lista(1, lista);
     print(lista)
   end"))
;; [1, 2, 3]
```

---

## 5. Punto 5 — Diccionarios

### 5.1 Representación interna

Un diccionario MathFlow se representa como una colección etiquetada de pares `(clave . valor)`:

```
dic = { (k1, v1), (k2, v2), ..., (kn, vn) }
```

La etiqueta distingue un diccionario de una lista (evita que un diccionario vacío se confunda con `vacio`). Las claves se buscan secuencialmente.

### 5.2 Gramática

```
<primitiva-diccionario> ::= "crear-diccionario" | "diccionario?" | "ref-diccionario"
                         |  "set-diccionario" | "claves" | "valores"
```

### 5.3 Primitivas

| Primitiva | Semántica |
|---|---|
| `crear-diccionario()` | Diccionario vacío. |
| `crear-diccionario(clave1, valor1, clave2, valor2, ...)` | Diccionario inicializado con los pares clave-valor dados, en el orden en que se escriben. |
| `diccionario?(x)` | `true` si `x` es un diccionario MathFlow válido. |
| `ref-diccionario(dic, clave)` | Valor asociado a `clave` en `dic`; `null` si la clave no existe. |
| `set-diccionario(dic, clave, valor)` | Asocia `valor` a `clave` en `dic` (la crea si no existía); devuelve el diccionario actualizado. |
| `claves(dic)` | Lista MathFlow con todas las claves de `dic`, en el orden de inserción. |
| `valores(dic)` | Lista MathFlow con todos los valores de `dic`, en el mismo orden que `claves`. |

### 5.4 Ejemplos `scan&parse` de cada producción

```racket
(scan&parse "crear-diccionario()")
(scan&parse "crear-diccionario(\"nombre\", \"Ana\", \"edad\", 34)")
(scan&parse "diccionario?(crear-diccionario())")
(scan&parse "ref-diccionario(crear-diccionario(\"nombre\", \"Ana\"), \"nombre\")")
(scan&parse "set-diccionario(crear-diccionario(\"nombre\", \"Ana\"), \"edad\", 34)")
(scan&parse "claves(crear-diccionario(\"id\", 101, \"nombre\", \"Carlos\"))")
(scan&parse "valores(crear-diccionario(\"id\", 101, \"nombre\", \"Carlos\"))")
```

### 5.5 Ejemplo de uso completo

```racket
(eval-program (scan&parse
  "begin
     var { pacientes = (crear-diccionario(\"id\", 101, \"nombre\", \"Carlos\", \"diagnostico\", \"Hipertension\")) };
     print(claves(pacientes));
     print(valores(pacientes))
   end"))
;; ["id", "nombre", "diagnostico"]
;; [101, "Carlos", "Hipertension"]
```

---

## 6. Punto 6 — Estructuras de control y recursión

Cada estructura de control en MathFlow es una **expresión**: siempre produce un valor y puede usarse dentro de otra expresión (por ejemplo, como argumento de una función, o como valor de retorno).

### 6.1 Condicional `if`

```
<expression> ::= "if" <expression> "then" <expression> "else" <expression> "end"
```

**Semántica:**
```
eval(if e1 then e2 else e3 end, env) =
    eval(e2, env)   si eval(e1, env) es verdadero (verdad dinámica)
    eval(e3, env)   en caso contrario
```
Solo se evalúa la rama correspondiente (evaluación perezosa de ramas). El anidamiento (`if ... else if ... end end`) se logra escribiendo un `if` completo como la expresión de la rama `else`.

**Ejemplo:**
```racket
(scan&parse "if >=(edad, 18) then print(\"Mayor de edad\") else print(\"Menor de edad\") end")
```

### 6.2 Selección múltiple `switch`

```
<expression>  ::= "switch" <expression> "{" { <caso-switch> }* "default" ":" <expression> "}"
<caso-switch> ::= "case" <expression> ":" <expression>
```

**Semántica:** se evalúa la expresión de control una única vez; luego se compara su valor, en orden, con el valor de cada `case`. Se evalúa y retorna la expresión del **primer** caso cuyo valor coincida (sin fallthrough a los casos siguientes); si ninguno coincide, se evalúa la cláusula `default`, que es obligatoria.

**Ejemplo:**
```racket
(scan&parse
  "switch color {
     case \"rojo\" : print(\"Detente\")
     case \"amarillo\" : print(\"Precaucion\")
     case \"verde\" : print(\"Sigue\")
     default : print(\"Color desconocido\")
   }")
```

### 6.3 Repetición condicional `while`

```
<expression> ::= "while" <expression> "do" <expression> "done"
```

**Semántica:** evalúa repetidamente la condición; mientras su valor sea verdadero (verdad dinámica), evalúa el cuerpo y vuelve a evaluar la condición en el ambiente resultante (de modo que toda actualización hecha dentro del cuerpo es visible en la siguiente iteración, y también después de terminado el ciclo). El valor del `while` completo es `null`.

**Ejemplo:**
```racket
(scan&parse
  "begin
     var { contador = (0) };
     while <(contador, 5) do
       contador = +(contador, 1)
     done
   end")
```

### 6.4 Iteración sobre estructuras `for`

```
<expression> ::= "for" <identifier> "in" <expression> "do" <expression> "done"
```

**Semántica:** la expresión tras `in` se evalúa una única vez y debe producir una lista MathFlow (sección 4). Por cada elemento de la lista, en orden, se liga el identificador a ese elemento y se evalúa el cuerpo; las actualizaciones hechas en una iteración son visibles en la siguiente. Si la lista está vacía, el cuerpo no se ejecuta ninguna vez. El valor del `for` completo es `null`.

**Ejemplo:**
```racket
(scan&parse
  "begin
     var { numeros = (crear-lista(1, crear-lista(2, crear-lista(3, vacio)))) };
     for n in numeros do
       print(n)
     done
   end")
```

### 6.5 Definición e invocación de funciones (incluye recursión)

Ver sección 3.6 — `func`, `return` e invocación mediante `(nombre arg1 arg2 ...)` se especifican allí porque forman parte del núcleo sintáctico del punto 2, y se usan tal cual para expresar recursión en el punto 6.

**Ejemplos de recursión:**
```racket
(scan&parse
  "func factorial (n) {
     if <=(n, 1) then return 1 else return *(n, (factorial -(n, 1))) end
   }")

(scan&parse
  "func fib (n) {
     if <=(n, 1) then return n else return +((fib -(n, 1)), (fib -(n, 2))) end
   }")
```

**Ejemplo de ejecución:**
```racket
(eval-program (scan&parse
  "begin
     func factorial (n) {
       if <=(n, 1) then return 1 else return *(n, (factorial -(n, 1))) end
     };
     print((factorial 5))
   end"))
;; 120
```

### 6.6 Secuenciación `begin ... end`

Ver sección 3.5. En el contexto del punto 6, `begin ... end` es la construcción que permite agrupar sentencias de control, declaraciones y llamadas a función en un único bloque ejecutable secuencialmente — es la forma habitual del cuerpo de un programa MathFlow completo.

---

## 7. Gramática completa (BNF, referencia)

```
<program>     ::= <expression>

<expression>  ::= <number>
               |  <string>
               |  <identifier> <identifier-tail>
               |  "true" | "false" | "null" | "vacio"
               |  <primitive> "(" { <expression> }*(",") ")"
               |  "if" <expression> "then" <expression> "else" <expression> "end"
               |  "switch" <expression> "{" { <caso-switch> }* "default" ":" <expression> "}"
               |  "while" <expression> "do" <expression> "done"
               |  "for" <identifier> "in" <expression> "do" <expression> "done"
               |  "var" "{" <declaraciones> "}"
               |  "const" "{" <declaraciones> "}"
               |  "func" <identifier> "(" { <identifier> }*(",") ")" "{" { <expression> }+(";") "}"
               |  "return" <expression>
               |  "begin" { <expression> }+(";") "end"
               |  "(" <expression> { <expression> }* ")"

<identifier-tail>    ::= ε | "=" <expression>
<declaraciones>      ::= <identifier> "=" "(" <expression> ")" <declaraciones-tail>
<declaraciones-tail> ::= ε | "," <declaraciones>
<caso-switch>        ::= "case" <expression> ":" <expression>

<primitive> ::= "+" | "-" | "*" | "/" | "%" | "add1" | "sub1"
             |  "<" | ">" | "<=" | ">=" | "==" | "<>"
             |  "and" | "or" | "not"
             |  "longitud" | "concatenar" | "print"
             |  "crear-lista" | "vacio?" | "lista?" | "cabeza" | "cola" | "ref-list" | "append" | "set-list"
             |  "crear-diccionario" | "diccionario?" | "ref-diccionario" | "set-diccionario" | "claves" | "valores"
```

---

## 8. Tabla resumen de primitivas

| Categoría | Primitivas |
|---|---|
| Aritméticas | `+`, `-`, `*`, `/`, `%`, `add1`, `sub1` |
| Relacionales | `<`, `>`, `<=`, `>=`, `==`, `<>` |
| Lógicas | `and`, `or`, `not` |
| Cadenas | `longitud`, `concatenar` |
| Entrada/salida | `print` |
| Listas | `crear-lista`, `vacio?`, `lista?`, `cabeza`, `cola`, `ref-list`, `append`, `set-list` |
| Diccionarios | `crear-diccionario`, `diccionario?`, `ref-diccionario`, `set-diccionario`, `claves`, `valores` |

---

## 9. Cómo probar la gramática

Para cualquier expresión `E` del lenguaje:

```racket
(scan&parse "E")                  ; muestra el árbol de sintaxis abstracta de E
(eval-program (scan&parse "E"))   ; evalúa E y muestra su resultado
```

Se recomienda envolver todo programa de prueba en `begin ... end`, y no dejar `;` antes del `end`/`}` de cierre de cada bloque, dado que el separador de sentencias se define como separador puro entre expresiones (`{<exp>}+(;)`), no como terminador.
