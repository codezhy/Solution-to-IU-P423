# Interp.rkt Notes

`interp-scheme` 是可以直接用于测试的函数。

`interp-R0`, `interp-R1` 等分别是对应 R0，R1语言等的解释器，由于 R0 是最基本的语言， R1 是 R0 之上的扩展， R2 又是 R1 的扩展，一层一层扩展，所以定义为类是比较合理的做法。继承关系如下：

```
R0 <- R1 <- R2 <- R3 <- R4 <- R6
```

## `interp-R0` 的定义

Racket 中搞面向对象的基本函数，

参考：
https://docs.racket-lang.org/guide/classes.html

1. `class`, `object%`

```
(class superclass-expr decl-or-expr ...)
```

函数 `class` 定义一个类，第一个参数是父类，第二个参数是当前类的定义。系统内置一个 root class 为 `object%`，就像一些编程语言中的 `object` 类一样。

所以

```
(define interp-R0
    (class object% 
    ...
    )
)
```

是定义类 `interp-R0`, 它继承自 root class `object%`。

2. `field` 

```
(field field-decl ...)
field-decl = (maybe-renamed default-value-expr)
maybe-renamed = id
 	 	      | renamed
renamed = (internal-id external-id)
```

`field` 声明 class 中的字段。

```
(gensym [base]) → symbol?
```

`gensym` 是返回一个“全新“的符号， 可以带一个参数，作为符号的前缀。(https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._gensym%29%29)

所以

```
(field (result (gensym 'result)))
```

声明一个字段 `result`, 它的值是类似于 `'result2831` 的符号。

3. `define/public`

```
(define/public (method parameter...) body)
```

声明一个 public 方法 `method`, 参数为 `parameter...`, 方法体为 `body`。

class `interp-R0` 中最重要的三个方法， `interp-scheme`, `interp-C`, `interp-x86`。都是以当前环境 `env` 为参数，返回一个函数，
函数以抽象代码树 `ast` 为参数，返回其解释。这三个方法分别针对编译过程中的三个阶段，`interp-scheme` 是解释scheme语言 (源语言)，`interp-C`是解释中间语言 (IR)，`interp-x86` 是解释 x86 汇编语言 (目标语言)。

`interp-R1`, `interp-R2` 等 class 就是继承并重载 `interp-scheme`, `interp-C`, `interp-x86` 三个方法。

## 其他

Essentials-Of-Compilation 书中描述的语言和代码的实现有点错乱。实际上是书中的 R1 (let) 对应 `interp-R0`，R2 (Booleans) 对应 `interp-R1`,
R3 (Tuples) 对应 `interp-R2`, R4 (Functions) 对应 `interp-R3`, R5 (Lambda) 对应 `interp-R4`, R6 (Typed Racket + Any) 对应 `interp-R6`，R7 (Untyped Racket) 没有对应。

# Chapter 2.5 Flatten Expressions

这节讲到 Racket 返回和处理多值用 `values` 和 `define-values` (及 `let*-values` 等)，注意，用 `values` 构造的值返回的不是 一个 `list`，是真正的多值，如果用返回的多值来调用函数，函数参数的个数必须和多值的个数匹配，用 `call-with-values` 如：

```
(define (function-with-3-params a b c)
  (+ a (- b c)))
(call-with-values (lambda () (values 1 2 3)) function-with-3-params)
```