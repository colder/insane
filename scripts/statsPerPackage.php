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

$fh = fopen("allResults.log", "r");

$packages = array();

$all = array_fill_keys(array_keys($groups), 0);

$total = 0;

while($fh && !feof($fh)) {
    $line = fgets($fh);

    ++$total;

    @list($cat, $meth) = explode("\t", $line);


    $package = implode(".", array_slice(explode(".", $meth), 0, -2));

    if (!$package) continue;

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
    foreach ($groups as $g => $tmp) {
        if ($g == "methods") continue;
        $data[$g] = round(100*$data[$g]/$data['methods'])."\\%";
    }

    vprintf("%-45s & ".implode(" & ", array_fill(0, count($groups), "%5s"))." \\\\ \n", array_merge(array($name), $data));
}

foreach ($groups as $g => $tmp) {
    if ($g == "methods") continue;
    $all[$g] = round(100*$all[$g]/$all['methods'])."\\%";
}
vprintf("%-45s & ".implode(" & ", array_fill(0, count($groups), "%5s"))." \\\\ \n", array_merge(array("TOTAL:"), $all));


echo "\n\n Total: ".$total." methods analyzed\n\n";
