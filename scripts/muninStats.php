#!/usr/bin/php
<?php

$cats = array(
    "methods" => 0,
    "pure" => 0,
    "impure" => 0,
    "condPure" => 0,
    "condImpure" => 0,
    "top" => 0,
    "bottom" => 0,
);

$fh = fopen(__DIR__."/../allResults.log", "r");

$total = 0;

while($fh && !feof($fh)) {
    $line = fgets($fh);

    ++$total;

    @list($cat, $meth) = explode("\t", $line);

    if (isset($cats[$cat])) {
        $cats[$cat] += 1;
    }
}


echo "pure.value    $cats[pure]\n";
echo "impure.value  $cats[impure]\n";
echo "cpure.value   $cats[condPure]\n";
echo "cimpure.value $cats[condImpure]\n";
echo "top.value     $cats[top]\n";
echo "bot.value     $cats[bottom]\n";
