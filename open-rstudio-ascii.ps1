param(
  [ValidatePattern("^[A-Za-z]$")]
  [string]$DriveLetter,
  [switch]$NoLaunch
)

$ErrorActionPreference = "Stop"
$substExe = Join-Path $env:SystemRoot "System32\subst.exe"

function Get-SubstMappings {
  $mappings = @{}

  foreach ($line in (& $substExe)) {
    if ($line -match '^([A-Z]):\\: => (.+)$') {
      $mappings[$matches[1]] = $matches[2]
    }
  }

  $mappings
}

function Find-RStudioExe {
  $candidates = @(
    "C:\Program Files\RStudio\rstudio.exe",
    "C:\Program Files\Posit\RStudio\rstudio.exe",
    "C:\Users\$env:USERNAME\AppData\Local\Programs\RStudio\rstudio.exe"
  )

  foreach ($candidate in $candidates) {
    if (Test-Path -LiteralPath $candidate) {
      return $candidate
    }
  }

  throw "Could not find rstudio.exe in the standard install locations."
}

$repoRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$repoRootResolved = (Resolve-Path -LiteralPath $repoRoot).Path
$projectFile = Get-ChildItem -LiteralPath $repoRootResolved -Filter *.Rproj | Select-Object -First 1

if (-not $projectFile) {
  throw "No .Rproj file was found in $repoRootResolved."
}

$preferredLetters = @("R", "S", "T", "U", "V", "W", "X", "Y", "Z", "Q", "P")
$mappings = Get-SubstMappings

if (-not $DriveLetter) {
  $existingMapping = $mappings.GetEnumerator() |
    Where-Object { $_.Value -eq $repoRootResolved } |
    Select-Object -First 1

  if ($existingMapping) {
    $DriveLetter = $existingMapping.Key
  } else {
    foreach ($candidate in $preferredLetters) {
      if (-not (Get-PSDrive -Name $candidate -PSProvider FileSystem -ErrorAction SilentlyContinue)) {
        $DriveLetter = $candidate
        break
      }
    }
  }
}

if (-not $DriveLetter) {
  throw "Could not find a free drive letter for a temporary ASCII alias."
}

$DriveLetter = $DriveLetter.ToUpper()
$driveRoot = "$DriveLetter`:"

if ($mappings.ContainsKey($DriveLetter)) {
  if ($mappings[$DriveLetter] -ne $repoRootResolved) {
    throw "$driveRoot is already mapped to $($mappings[$DriveLetter]). Choose another drive letter."
  }
} elseif (Get-PSDrive -Name $DriveLetter -PSProvider FileSystem -ErrorAction SilentlyContinue) {
  throw "$driveRoot is already in use. Choose another drive letter."
} else {
  & $substExe $driveRoot $repoRootResolved | Out-Null
}

$aliasProjectPath = "$driveRoot\$($projectFile.Name)"

if (-not (Test-Path -LiteralPath $aliasProjectPath)) {
  throw "Failed to create a usable alias at $aliasProjectPath."
}

Write-Host "ASCII alias ready:" $aliasProjectPath
Write-Host "Cleanup command:" "subst $driveRoot /d"

if (-not $NoLaunch) {
  $rstudioExe = Find-RStudioExe
  Start-Process -FilePath $rstudioExe -ArgumentList ('"' + $aliasProjectPath + '"')
}
