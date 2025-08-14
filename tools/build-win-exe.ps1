<#
 Made by 0x1000007e

 Build a standalone rulu.exe on Windows using srlua + glue.

 Usage (PowerShell):
   powershell -ExecutionPolicy Bypass -File tools\build-win-exe.ps1

 This script will:
   - Ensure dist\rulu.lua is built (runs `lua tools/bundle.lua` if needed)
   - Download srlua.exe and glue.exe into tools\srlua\ (from a few known mirrors)
   - Produce rulu.exe in the repo root using glue

 Note: You need a Lua interpreter on PATH to run the bundler step.
#>

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Ensure-DistBundle {
  if (-not (Test-Path -LiteralPath 'dist\\rulu.lua')) {
    Write-Host 'dist\\rulu.lua not found; running bundler...'
    & lua 'tools\\bundle.lua'
  } else {
    Write-Host 'dist\\rulu.lua found.'
  }
}

function New-Dir($path) {
  if (-not (Test-Path -LiteralPath $path)) {
    New-Item -ItemType Directory -Path $path | Out-Null
  }
}

function Try-Download($urls, $outPath) {
  foreach ($u in $urls) {
    try {
      Write-Host "Downloading $u -> $outPath"
      Invoke-WebRequest -Uri $u -OutFile $outPath -UseBasicParsing
      if ((Get-Item $outPath).Length -gt 0) { return $true }
    } catch {
      Write-Warning "Failed: $u ($_ )"
    }
  }
  return $false
}

function Ensure-SRLua {
  $dir = 'tools\\srlua'
  New-Dir $dir
  $srlua = Join-Path $dir 'srlua.exe'
  $glue = Join-Path $dir 'glue.exe'

  $srluaUrls = @(
    'https://raw.githubusercontent.com/noahp/srlua-mingw/master/srlua.exe'
  )
  $glueUrls = @(
    'https://raw.githubusercontent.com/noahp/srlua-mingw/master/glue.exe'
  )

  if (-not (Test-Path -LiteralPath $srlua)) {
    if (-not (Try-Download $srluaUrls $srlua)) {
      throw 'Could not download srlua.exe automatically. Update URLs in tools\\build-win-exe.ps1 or place srlua.exe into tools\\srlua\\ manually.'
    }
  }
  if (-not (Test-Path -LiteralPath $glue)) {
    if (-not (Try-Download $glueUrls $glue)) {
      throw 'Could not download glue.exe automatically. Update URLs in tools\\build-win-exe.ps1 or place glue.exe into tools\\srlua\\ manually.'
    }
  }

  return @{ Srlua = $srlua; Glue = $glue }
}

function Build-Exe {
  param(
    [string]$srlua,
    [string]$glue
  )
  $bundle = 'dist\\rulu.lua'
  if (-not (Test-Path -LiteralPath $bundle)) { throw 'dist\\rulu.lua missing' }
  $out = 'rulu.exe'
  Write-Host "Building $out using glue..."
  & $glue $srlua $bundle $out
  if (-not (Test-Path -LiteralPath $out)) { throw 'Failed to create rulu.exe' }
  Write-Host "Created $out"
}

Ensure-DistBundle
$bins = Ensure-SRLua
Build-Exe -srlua $bins.Srlua -glue $bins.Glue


