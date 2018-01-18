# Название проекта

### Материалы
[Ссылка на таблицу с данными](https://docs.google.com/spreadsheets/d/1Pdpd7f0ApJH8lMs1G6f5I2mgbSXEPdUsPxuw_ftExy4/edit#gid=0)
[Ссылка на папку с разными таблицами](https://github.com/s-klimenko/CxG/tree/master/data)  
[Ссылка на код, здесь целиком с комментарием](https://github.com/s-klimenko/CxG/blob/master/project.R) ( или ниже по тексту отдельными блоками )

## Рабочая гипотеза

В русском языке существуют три синонимичных глагола: наклониться, нагнуться, покоситься. При помощи этого исследования, мы хотим узнать какие параметры влияют на выбор конкретного глагола.

Важно отметить, что глагол покоситься имеет два значения:
* покоситься.1 - искривиться, наклониться, стать косым
* покоситься.2 - посмотреть искоса, вбок.

Обзор данных показал, что глагол покоситься.1, в отличие от остальных, употребляется исключительно с неодушевленными субъектами. Различие между контекстами наклониться и нагнуться было решено выяснить в процессе исследования.


## Данные

### Материал исследования
Для исследования из Национального корпуса русского языка было собрано:
* 164 примеров с глаголом наклониться
* 164 примеров с глаголом нагнуться
* 164 примеров с глаголом покоситься (из которых 44 оказалось со значением покоситься.1, а 120 со значением покоситься.2)

Наибольшую трудность составила разметка цели и направления. В целом, целью считались конструкции типа "наклониться сделать", "наклониться чтобы сделать". "наклониться и сделать" только в случае, если действие нельзя сделать без наклона (сравни "наклонился и поднял камень" и "наклонился и сказал")
Конструкциями направления чаще всего считались конструкции с предлогом, кроме за.
Эти теги не являются взамоисключающими.

### Факторы выбора конструкции

* Целевой переменной являлся выбранный глагол (наклониться, нагнуться, покоситься.1, покоситься.2)
Кроме того, было выделены следующие переменные:
* наличие или отсутствие предлога (Маша нагнулась vs Маша нагнулась к Кате) 
* значение направления или цели (наклониться к ручью vs наклониться за кошельком)
* наличие инструмента (покоситься глазами, наклониться всем телом)
* одушевленность субъекта (забор наклонился vs Маша наклонилась)
* одушевленность объекта (Вася наклонился к Пете vs Вася наклонился к ручью)
* форма глагола (финитная, инфинитив, императив, деепричастие, причастие)


## Анализ: дескриптивная статистика

* Форма
			
глагол|финит|деепр|императив|инфинитив|прич
--|--|--|--|--|--
нагнуться|119|32|3|9|1
наклониться|113|39|3|7|2
покоситься.1|6|2|||36
покоситься.2|107|13|||
итого|345|86|6|16|39

Покоситься.1 больше всех встречается с причастиями.


* Одушевленность субъекта

глагол|n|y
--|--|--
нагнуться||164
наклониться|4|160
покоситься.1|43|1
покоситься.2||120
итого|47|445

Нагнуться и покоситься.1 не встречается с неодушевленными субъектами, наклониться тоже практически не. Наклониться наоборот, почти не встречается с одушевленными.


* Одушевленность объекта

глагол|None|n|y
-|--|--|--
нагнуться|109|36|19
наклониться|63|55|46
покоситься.1|41|3|
покоситься.2|10|22|88
итого|223|116|153

Покоситься.1 не встречается с одушевленными объектами и почти не встречается с неодушевленными. Нагнуться преимущественно встречается без объекта. Покоситься -- с одушевленным объектом.


* Направление и цель

глагол|DnGn|DnGy|DyGn|DyGy				
--|--|--|--|--
нагнуться|47|71|32|14	
наклониться|38|35|60|31	
покоситься.1|41||3|	
покоситься.2|9||111|	
итого|135|106|206|45

Покоситься.2 практически всегда имеет значение только направления. Покоситься.1 не имеет обычно ни направления, ни цели. Наклониться чаще встречается только с направлением, а нагнуться -- с целью. 

![alt text](corr.png)

Как видно из матрицы корреляции с целевой переменной наиболее коррелируют значение цели, одушевленность субъекта и наличие предлога. Кроме того, есть небольшая корреляция с формой глагола (финитная, инфинитив, деепричастие) и одушевленностью объекта.

## Мультифакторный анализ


При использовании всех коррелирующих переменных, дерево решений принимает следующий вид:
![alt text](tree1.png)
Оно же:
![alt text](tree2.png)
Исключаем форму, оставляем только цель, одушевленность объекта и субъекта и наличие предлога:
![alt text](tree3.png)


* Глагол покоситься.1 употребляется исключительно с неодушевленным субъектом.
* Глагол покоситься.2 не употребляется в значении цели.
* Глагол наклониться преимущественно употребляется со значением цели.
* Глагол нагнуться чаще употреблятся с неодушевленным субъектом.

## Содержательный лингвистический анализ результатов статистического анализа
![alt text](gini.png)

Действительно, форма глагола практически не влияет на результат, в то время как цель, одушевленность и наличие предлога являются важными переменными.

## Обсуждение использованных квантитативных методов
Accuracy составила 60%. Скорее всего такое маленькое значение получилось из-за маленького количества материала и неравномерного распределения выборки.
