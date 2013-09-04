# simple_shift_reduce_parsing

Shift-reduce parser based on Nivre (2004).

## Usage

```sh
# command line help
% lein run -m simple_shift_reduce_parsing.core --help

# training the model from annotated sentences
% lein run --mode train --training-filename train.txt --k 10

# parse test sentences
% lein run --mode test --test-filename test.txt

# evaluate the system performance
% lein run --mode eval --test-filename test.txt
```

## About this model
Nivre (2004)のarc-eagerなshift-reduce parserです。actionの選択にはFOBOSによるSVM([syou6162/fobos_clj](https://github.com/syou6162/fobos_clj)、[syou6162/fobos_multiclass_clj](https://github.com/syou6162/fobos_multiclass_clj))を利用しています(one versus restで4値分類)。L1正則化がかかっているので、割とコンパクトでそこそこの性能が出るモデルができあがるはずです。

素性には(ありがちな)スタックとキューの単語の表層や品詞、親子関係や以前のactionなどを入れています。詳しくは[feature.clj](https://github.com/syou6162/simple_shift_reduce_parsing/blob/master/src/simple_shift_reduce_parsing/feature.clj)を見てください。FOBOSにはカーネルを利用することができないので、組み合わせ素性を陽に入れています(そこの生成でdecode時のスピードが少し犠牲になっている)。

## File format conversion

```sh
# mst format => conll format (with unlabeld)
% python bin/mst2conll.py data/test.lab | awk '{if ($0 != "") {print $1"\t"$2"\t"$3"\t"$4"\t"$5"\t"$6"\t"$7"\t_"} else {print}}' > gold.txt
```

## Error analysis

```sh
# you can use 'whatswrong' (https://code.google.com/p/whatswrong/) to analyze the parsing errors
% java -jar bin/whatswrong-0.2.3-standalone.jar
```

## License

Copyright (C) 2012 Yasuhisa Yoshida

Distributed under the Eclipse Public License, the same as Clojure.
