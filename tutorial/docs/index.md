# Welcome to the detectEr tutorial!
---

![GitHub Workflow Status](https://img.shields.io/github/workflow/status/duncanatt/detecter/Build?logo=GitHub&logoColor=white)
![GitHub last commit](https://img.shields.io/github/last-commit/duncanatt/detecter)
![GitHub](https://img.shields.io/badge/version-0.9-yellow)
![GitHub](https://img.shields.io/github/license/duncanatt/detecter)

<!-- ![GitHub issues](https://img.shields.io/github/issues/duncanatt/detecter) -->
<!-- ![GitHub closed issues](https://img.shields.io/github/issues-closed/duncanatt/detecter) -->

## What is detectEr?

detectEr is a runtime verification tool for asynchronous component systems that run on the Erlang Virtual Machine.
It also supports monitoring systems that can execute outside of the EVM, so long as these can produce traces that are formatted in a way that is parsable by detectEr.
The tool itself is developed in Erlang, and is the product of five years of theoretical and practical development.

## Tutorial videos

We have a three-part tutorial series on YouTube that gives an overview of how to use detectEr.
Use these videos as a complementary learning material to this tutorial.

[Part 1: Diving In](https://youtu.be/ADc2HM63ppQ){ .md-button .md-button--primary}
[Part 2: Describing Program Correctness](https://youtu.be/xAml5kLBaBQ){ .md-button .md-button--primary}
[Part 3: Instrumenting the Program](https://youtu.be/OUwKF38GRbM){ .md-button .md-button--primary}

## How it works

There are two ingredients required for detectEr to work.
The first one is a script file containing specifications of the properties one would like to monitor.
Properties in detectEr are expressed in sHML---a runtime monitorable syntactic subset of the more expressive modal μ-calculus---used to specify *safety properties*.
detectEr compiles these specifications down to executable Erlang code that analyses program events to reach monitoring verdicts that correspond to property violations.

The second ingredient detectEr requires is the program to be monitored, also called the program under scrutiny.
detectEr instruments the program with the aforementioned analyser code.
detectEr supports three instrumentation methods: inline, outline and offline.
In inline instrumentation, detectEr statically instruments the program under scrutiny by weaving the executable analyser instructions via code injection.
The ensuing runtime analysis then takes place as the weaved program components execute.
Outline monitors enables detectEr to take a dynamic instrumentation approach that treats the system as a black box.
It leverages the tracing infrastructure provided by the EVM to gather trace events that are reported to independent component analysers.
detectEr also extends outline instrumentation to the offline case, where events read from a trace dump are replayed to emulate the interaction between concurrent system components.

This tutorial overviews the inner workings of detectEr.
It showcases the inline, outline, and offline monitoring functionality of the tool, demonstrating how each can be employed to monitor programs that are subject to specific deployment and runtime constraints.
We highly encourage you to consult the [paper]() that accompanies this tutorial, since it complements many of the concepts covered here.
Interested readers are also referred to the list of publications that follows.

## Publications

[COORDINATION 2022](http://staff.um.edu.mt/afra1/papers/Coord2022b.pdf){ .md-button .md-button--secondary}
[FORTE 2021](http://staff.um.edu.mt/afra1/papers/forte-tutorial-2021.pdf){ .md-button .md-button--secondary}
[POPL 2019](http://staff.um.edu.mt/afra1/papers/popl19.pdf){ .md-button .md-button--secondary}
[BETTY 2017](http://staff.um.edu.mt/afra1/papers/betty-book.pdf){ .md-button .md-button--secondary}
[FMSD 2017](http://staff.um.edu.mt/afra1/papers/rvhmlr-jour.pdf){ .md-button .md-button--secondary}
[RV 2016](http://staff.um.edu.mt/afra1/papers/rv2016.pdf){ .md-button .md-button--secondary}

List of other publications to be added later :)

## Help us

Please help us improve this tutorial and the tool detectEr!
We would appreciate if any typos or bugs found are reported on the [issues](https://github.com/duncanatt/detecter/issues) page on GitHub.

## Our group

detectEr is part of the [TheoFoMon](http://icetcs.ru.is/theofomon) and [MoVeMnt](https://sites.google.com/view/antonisachilleos/movemnt) projects funded by the [Icelandic Research Fund](https://en.rannis.is).
Duncan Paul Attard---the principal developer of the detectEr---is a researcher with the ICE-TCS research group at Reykjavík University.
He has worked as a software developer in the Telecoms and Online payment industry for seven years prior to joining the academia.

The research group consists of these members who actively contribute towards the theoretical and practical developments of detectEr:

* [Luca Aceto](http://www.ru.is/faculty/luca)
* [Antonis Achilleos](https://sites.google.com/view/antonisachilleos/home)
* [Elli Anastasiadi](https://github.com/l0e42)
* [Duncan Paul Attard](http://duncanatt.github.io)
* [Adrian Francalanza](http://staff.um.edu.mt/afra1)
* [Karoliina Lehtinen](http://www.pageperso.lif.univ-mrs.fr/~karoliina.lehtinen)
* [Anna Ingólfsdóttir](http://www.ru.is/kennarar/annai)

## Acknowledgements

We would like to thank [Matthew Alan Le Brun](https://github.com/MatthewAlanLeBrun) for his comments and feedback.

<!-- :material-heart:{ .heart } -->
<!-- {: .center } -->