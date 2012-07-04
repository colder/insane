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

$fh = fopen("allResults.log", "r");

$packages = array();

$total = 0;

while($fh && !feof($fh)) {
    $line = fgets($fh);

    ++$total;

    @list($cat, $meth) = explode("\t", $line);


    $package = implode(".", array_slice(explode(".", $meth), 0, -2));

    if (!$package) continue;

    if (!isset($packages[$package])) {
        $packages[$package] = $cats;
    }

    $packages[$package]["methods"] += 1;

    $packages[$package][$cat] += 1;
}

ksort($packages);

vprintf("%-40s & %4s & %4s & %4s & %4s & %4s & %4s & %4s \\\\ \n", array("", "#M", "P", "NP", "CP", "CNP", "Top", "Bot"));
foreach ($packages as $name => $data) {
    vprintf("%-40s & %4d & %4d & %4d & %4d & %4d & %4d & %4d \\\\ \n", array_merge(array($name), $data));
}


echo "\n\n Total: ".$total." methods analyzed";
