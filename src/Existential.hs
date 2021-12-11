{-# LANGUAGE RankNTypes #-}
module Existential where

-- Описание экзистенциального типа \exists a . (a, Integer -> a -> a, a -> (a, Integer))
-- Это абстрактный тип данных, реализующий интерфейс стека с тремя операциями:
-- empty, push и pop. Для упрощения мы рассматриваем только целочисленный стек.
-- Здесь мы используем способ представления квантора существования в
-- интуиционистской логике 2-го порядка через квантор всеобщности:
-- \exists a . s = \forall b . ((\forall a . (s -> b)) -> b)
-- Соответственно, описание типа превращается в такое:
newtype AbstractStack = AS (forall b . (forall a . ( a -- Начальный стек
                                                , Integer -> a -> a -- Положить на стек
                                                , a -> (a, Integer) -- Снять со стека
                                                ) -> b) -> b)

-- Давайте для начала покажем, как использовать тип AbstractStack. Напишем программу,
-- которая, например, кладет пару чисел на стек, потом снимает их оттуда и суммирует.
-- Заметьте, в терминологии языка Java мы могли бы сказать, что функция
-- compute получает на вход класс (но не объект), реализующий интерфейс AbstractStack.
compute :: AbstractStack -> Integer
compute stack =
-- Возьмем AbstractStack, в нем единственный конструктор AS. Раскроем его:
  case stack of
-- и распишем abstype из конспекта лекций (не забудьте, что тут типизация по Карри,
-- поэтому типовые абстракции/применения не пишутся):
    AS r -> r x where
-- А теперь выпишем собственно тело функции, использующей абстрактный тип данных.
-- Сам АТД -- единственный аргумент функции x
      x (empty, push, pop) = 
        let (stk, v) = pop (push 12 $ push 5 empty) in
          let (stk2, v2) = pop stk in v + v2

-- Теперь пришла пора реализовывать эти функции:
listPush :: a -> [a] -> [a]
listPush i l = i : l

listPop :: [a] -> ([a], a)
listPop (i : l) = (l, i)
listPop [] = undefined 

listEmpty :: [a]
listEmpty = []

-- Наконец, запакуем функции внутрь абстрактного типа данных stack.
-- Сравните это с реализацией pack из конспекта лекций
stack :: AbstractStack
stack = AS (\t -> t (listEmpty, listPush, listPop))

-- В переменной stack находится некоторая реализация интерфейса,
-- однако, значение типа AbstractStack скрывает подробности внутри.
-- Поясним это на примере. Допустим, мы решим расширить
-- представление данных, и добавить в него счетчик глубины стека:
clistPush :: a -> ([a], Integer) -> ([a], Integer)
clistPush i (l, n) = (i:l, n + 1)

clistPop :: ([a], Integer) -> (([a], Integer), a)
clistPop (i : l, n) = ((l, n - 1), i)
clistPop ([], _) = undefined 

clistEmpty :: ([a], Integer)
clistEmpty = ([], 0)

-- Будь это обычной упорядоченной тройкой, это изменение немедленно
-- отразилось бы на типе всей тройки. Однако, тут тип итогового стека
-- будет тот же самый, все подробности скрыты.
stack2 :: AbstractStack
stack2 = AS (\t -> t (clistEmpty, clistPush, clistPop))

main :: IO ()
main =
  -- ну и теперь применим и убедимся, что все работает.
  print [compute currentStack | currentStack <- [stack, stack2]]


{-

newtype AbstractQueue = 
  AQ (forall b . (forall a . 
    (a, (Integer, Integer) -> a -> a, a -> (a, Integer)) -> b) -> b)

-- Теперь пришла пора реализовывать эти функции:
queuePush :: a -> [(Int, a)] -> [(Int, a)]
queuePush i l = i : l

queuePop :: [a] -> ([a], a)
queuePop (i : l) = (l, i)
queuePop [] = undefined 

queueEmpty :: [a]
queueEmpty = []
-}









