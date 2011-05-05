<?php
$mail = '/home/colder/Mail/';

setlocale(LC_ALL, "en_US.utf8");

$news = array();
foreach(glob("$mail*/new/*") as $f) {
    $news[$f] = $f;
}

foreach(glob("$mail*/*/new/*") as $f) {
    $news[$f] = $f;
}


function decode($in) {
    $out = "";

    foreach(imap_mime_header_decode($in) as $part) {
        $out .= $part->text;
    }

    return htmlspecialchars(@iconv("UTF-8", "ASCII//IGNORE//TRANSLIT", $out));
}

function getMailInfo($file) {
    global $mail;

    $content = file_get_contents($file);

    $from = "";
    if (preg_match("/^From: ([^<>\n]+)/m", $content, $match)) {
        $from = decode($match[1]);
    } else if(preg_match("/^From: <(.+)>/m", $content, $match)) {
        $from = decode($match[1]);
    }

    $subject = "";
    if (preg_match("/^Subject: (.+)$/m", $content, $match)) {
        $subject = decode($match[1]);
    }

    $folder = str_replace($mail, '', dirname(dirname($file)));

    return array("folder" => $folder,
                 "from" => $from ?: "N/A",
                 "subject" => $subject ?: "N/A");
}

$force = isset($argv[1]) ? $argv[1] == '-f' : false;

$path = dirname(__FILE__).'/lastcheck';
if (!file_exists($path)) {
    touch($path);
}

// filter
foreach ($news as $k => $n) {
    if (preg_match("/Duplicates/", $n) != false) {
        unset($news[$k]);
    }
}

$all_news = $news;

$alreadySeen = @unserialize(file_get_contents($path));

if (!$alreadySeen) {
    $alreadySeen = array();
}

file_put_contents($path, serialize($all_news));

$title = count($news).' new mail'.((count($news) > 1) ? "s" : "");

foreach($alreadySeen as $seen) {
    if (isset($news[$seen])) unset($news[$seen]);
}

if (!empty($news)) {
    $mails = array();

    foreach($news as $n) {
        $data = getMailInfo($n);

        if (!isset($mails[$data['folder']])) {
            $mails[$data['folder']] = array();
        }

        $mails[$data['folder']][] = $data;
    }

    // formatting
    $content = "";
    foreach($mails as $folder => $ms) {
        $content .= "<b>[$folder]</b>\n";

        foreach($ms as $m) {
            $content .= "• <i>$m[from]</i>\n$m[subject]\n";
        }

        $content .= "\n";
    }

    if ($force) {
        echo $title;
        echo $content;
    }
    $command = "/usr/bin/notify-send -t ".(5000+1000*count($news))." -u low -i stock_mail-unread ".escapeshellarg($title)." ".escapeshellarg($content)."";
    //$command = "/usr/local/bin/growlnotify -a \"Mail\" -t ".escapeshellarg($title)." -m ".escapeshellarg($notification)."";
    exec($command);
}
