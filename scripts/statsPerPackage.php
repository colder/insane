#!/usr/bin/php
<?php
$typeToGroup = array(
    "methods"    => "methods",
    "pure"       => "pure",
    "impure"     => "impure",
    "condPure"   => "condPure",
    "condImpure" => "impure",
    "top"        => "top",
    "bottom"     => "impure",
);

$groups = array(
    "methods"   => "#M",
    "pure"      => "P",
    "condPure"  => "CP",
    "impure"    => "NP",
    "top"       => "Top",
);

function packageMap($p) {
    if (strpos($p, "scala.annotation") !== false) {
        return "scala.annotation";
    }

    if (strpos($p, "scala.collection.mutable") !== false) {
        return $p;
    }

    if (strpos($p, "scala.collection.immutable") !== false) {
        return $p;
    }

    if (strpos($p, "scala.collection.parallel") !== false) {
        return "scala.collection.parallel";
    }

    if (strpos($p, "scala.sys") !== false) {
        return "scala.sys";
    }

    if (strpos($p, "scala.util.parsing") !== false) {
        return "scala.util.parsing";
    }

    if (strpos($p, "scala.util") !== false) {
        return "scala.util";
    }

    if (strpos($p, "scala.reflect") !== false) {
        return "scala.reflect";
    }

    if (strpos($p, "scala.xml") !== false) {
        return "scala.xml";
    }

    if (strpos($p, "scala.concurrent") !== false) {
        return "scala.xml";
    }

    return $p;
}

$fh = fopen(isset($argv[1]) ? $argv[1] : "allResults.log", "r");

$packages = array();

$all = array_fill_keys(array_keys($groups), 0);

$total = 0;

while($fh && !feof($fh)) {
    $line = fgets($fh);


    @list($cat, $meth) = explode("\t", $line);


    $package = implode(".", array_slice(explode(".", $meth), 0, -2));

    $package = packageMap($package);

    if (!$package) continue;

    ++$total;

    if (!isset($packages[$package])) {
        $packages[$package] = array_fill_keys(array_keys($groups), 0);
    }

    $packages[$package]["methods"] += 1;
    $all["methods"] += 1;

    $packages[$package][$typeToGroup[$cat]] += 1;
    $all[$typeToGroup[$cat]] += 1;
}

ksort($packages);

vprintf("%-45s & ".implode(" & ", array_fill(0, count($groups), "%5s"))." \\\\\n", array_merge(array(""), $groups));
foreach ($packages as $name => $data) {
    vprintf("%-45s & ".implode(" & ", array_fill(0, count($groups), "%5s"))." \\\\ \n", array_merge(array($name), $data));
    foreach ($groups as $g => $tmp) {
        if ($g == "methods") continue;
        $data[$g] = round(100*$data[$g]/$data['methods'])."\\%";
    }

    vprintf("%-45s & ".implode(" & ", array_fill(0, count($groups), "%5s"))." \\\\ \n", array_merge(array($name), $data));
}

vprintf("%-45s & ".implode(" & ", array_fill(0, count($groups), "%5s"))." \\\\ \n", array_merge(array("TOTAL:"), $all));
foreach ($groups as $g => $tmp) {
    if ($g == "methods") continue;
    $all[$g] = ($all['methods'] > 0 ? round(100*$all[$g]/$all['methods']) : 0)."\\%";
}
vprintf("%-45s & ".implode(" & ", array_fill(0, count($groups), "%5s"))." \\\\ \n", array_merge(array("TOTAL:"), $all));


echo "\n";
