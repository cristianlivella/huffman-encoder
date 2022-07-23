<?php

const SPECIAL_CHARS = [
    PHP_EOL => 'Newline',
    ' ' => 'Space'
];

$fileContent = file_get_contents('../file.txt');
$fileContent = str_split($fileContent);

$countRepetitions = [];

foreach ($fileContent as $char) {
  if (!isset($countRepetitions[$char])) {
    $countRepetitions[$char] = 0;
  }
  $countRepetitions[$char]++;
}

$pairs = [];

foreach ($countRepetitions as $char => $count) {
    $char = '#\\' . (isset(SPECIAL_CHARS[$char]) ? SPECIAL_CHARS[$char] : $char);

    $pairs[] = '(' . $char . ' . ' . $count . ')';
}

echo '(' . implode(' ', $pairs) . ')' . PHP_EOL;
