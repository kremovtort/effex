## Enum
```rust
enum List<A> {
  Nil,
  Cons { head: A, tail: &List<A> }
}
```
## Struct
```rust
struct Pair<A, B> {
  first: A,
  second: B
}
```
## GADT
```rust
enum Expr<A> {
  Bool(bool): Expr<bool>,
  Int(i64): Expr<i64>,
  <X> List(Vec<Expr<X>>): Expr<Vec<X>>
}
```
## Кортежи
```rust
let x: (boolean, i32, string) = (false, 2: i32, "my string");
println!("({}, {}, {})", x.1, x.2, x.3);
```

# Модификаторы мутабельности
По умолчанию в определении типа не задаётся. Вместо этого (им)мутабельность задаётся при определении типа переменной и наследуется структурой. Но при этом иммутабельность поля можно задать при определении типа
```rust
struct Pair<A, B> {
  first: A,
  second: B,
}

let x = Pair { first = true, second = 2: i32 };
x.first = false; // ошибка, т.к. x иммутабельно, то и его поля невозможно изменить
let mut x = Pair { first = true, second = 2: i32 };
x.first = false; // ошибки нет

struct ImutPair<A, B> {
  first: const A,
  second: const B,
}

let mut x = ImutPair { first = true, second = 2: i32 };
x.first = false; // ошибка компиляции, поле first иммутабельно
```

Типы могут быть параметризованы скоупами:
```rust
struct RefPair<'s, A, B> {
  first: &'s A,
  second: &'s B,
}
```
