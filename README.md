# Лабораторная работа 3

Автор: Голиков Андрей Сергеевич P34092 335126

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

## Использование

```bash
stack build # сборка

stack exec lab3-exe <start> <step> <end> <method> <debug>
```
- Параметры `start`, `step`, `end` настраивают генератор точек интерполяции.

- `method` - введите `0` для выбора линейной интерполяции, `1` для интерполяции методом Лагранжа и `2` для использования обеих методов

- `debug` - При установке `1` значения забираются из файла `test.txt`, а точки выводятся в виде графика, иначе - вывод согласно тз.


## Реализация

Полный код представлен в `src`. Ниже продемонстрированы наиболее важные участки кода

### Линейная интерполяция

```haskell
linInter :: (Fractional a, Ord a, Show a) => [Point a] -> [a] -> [a]
linInter p l = fst $ help (const 0) p l
  where
    help :: (Fractional a, Ord a, Show a) => Func a -> [Point a] -> [a] -> ([a], Func a)
    help f points list = case points of
      [] -> (map f list, f)
      [(x1, y1)] ->
        let newF x = if f x1 /= y1 then y1 else f x in (map newF list, newF)
      ((x1, y1) : (x2, y2) : _) -> case list of
        [] -> ([], f)
        (cur : gen)
          | cur < x1 ->
              let newF = if f x1 /= y1 then linearF else f
                  (rest, finalF) = help newF points gen
               in (newF cur : rest, finalF)
          | cur >= x1 && cur <= x2 ->
              let newF x
                    | x >= x1 = linearF x
                    | otherwise = f x
                  (rest, finalF) = help newF points gen
               in (newF cur : rest, finalF)
          | cur > x2 ->
              let (rest, finalF) = help f (tail points) gen
               in (f cur : rest, finalF)
        where
          linearF x = k * x + b
          k = (y1 - y2) / (x1 - x2)
          b = y1 - k * x1
```

### Метод Лагранжа

```haskell
lagInter :: (Fractional a, Ord a, Show a) => [Point a] -> [a] -> [a]
lagInter = help []
  where
    help :: (Fractional a, Ord a, Show a) => [Point a] -> [Point a] -> [a] -> [a]
    help _ _ [] = []
    help acc points gen@(curr : restGen) = case points of
      [] -> map predict gen
      (point@(x, y) : restP) -> case compare curr x of
        LT -> predict curr : help acc points restGen
        EQ -> y : help acc points restGen
        GT -> help (point : acc) restP gen
      where
        ps = if null points then acc else head points : acc
        predict a = sum (map (calcCoef a) ps)
        xs = map fst ps

        calcCoef t (xi, yi) = yi * foldl (foldWithout t xi) 1 xs / foldl (foldWithout xi xi) 1 xs

        foldWithout x skip accum xi
          | skip == xi = accum
          | otherwise = accum * (x - xi)
```

## Пример использования

```
$ stack exec lab3-exe 0 1 5 0 0
> 1 2
> 3 0
(0.0,3.0)
(1.0,2.0)
(2.0,1.0)
(3.0,0.0)
> 4 10
(4.0,10.0)
> <ctrll-D>
(5.0,20.0)
```

## Тестирование

```
lab3> test (suite: lab3-test)
                  
Cheking linear interpolation ...
+++ OK, passed 1000 tests.
Cheking lagrangia interpolation ...
+++ OK, passed 1000 tests.



lab3> Test suite lab3-test passed
Completed 2 action(s).
```

## Вывод

Довольно интересно работать с ленивыми вычислениями, но, кажется, они обрубают возможность делать основные обрабатывающие функции хвосторекурсивными, из-за чего мне становится страшно за стек. Так же, не очень понятно, зачем в требованиях указана возможность запускать два метода одновременно, если в однопоточной программе вывод значений не сможет перемешаться по определению независимо от лени.
