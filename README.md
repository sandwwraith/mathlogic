# How to run

There are 5 main classes for each homework. You can run them from sbt.
Main class names are `design.sandwwraith.mlhw.Task<number>`,
testdata will be looked up in `testdata/HW<number>`

E.g. `sbt "runMain design.sandwwraith.mlhw.Task2 contra.in"` from bash will
launch task 2 on `testdata/HW2/contra.in` input file. (Though it's better to launch
it directly from sbt shell to keep scala VM alive between invocations)

Task 5 doesn't accept filename, just two numbers separated by space.
For tasks 2-5 you can add "andCheck" as last command argument to verify generated proof.

> Tasks source: https://github.com/shd/logic2015/blob/master/homework.pdf
