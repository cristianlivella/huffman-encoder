<?php

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
  $pairs[] = '(' . str_replace('"', '\'', json_encode($char)) . ', ' . $count . ')';
}

echo '[' . implode(', ', $pairs) . ']' . PHP_EOL;
