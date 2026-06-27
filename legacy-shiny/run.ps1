# Launches the Artha HE Shiny app in your browser.
# Usage:  right-click > Run with PowerShell,  or:  ./run.ps1
$ErrorActionPreference = "Stop"
$Rscript = "C:\Program Files\R\R-4.4.3\bin\Rscript.exe"
if (-not (Test-Path $Rscript)) {
    $found = Get-ChildItem "C:\Program Files\R" -Directory |
             Sort-Object Name -Descending | Select-Object -First 1
    if ($found) { $Rscript = Join-Path $found.FullName "bin\Rscript.exe" }
}
$AppDir = $PSScriptRoot
Write-Host "Starting Artha HE ..." -ForegroundColor Green
& $Rscript -e "shiny::runApp('$($AppDir -replace '\\','/')', launch.browser = TRUE, port = 7654)"
