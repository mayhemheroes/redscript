param (
    [string]$zipPath
)

if (-not $zipPath) {
    Write-Error "No output path provided. Please provide a path to write the zip archive to."
    exit 1
}

if (-not (Test-Path "target/release/scc.exe")) {
    Write-Error "scc.exe not found in target/release. Please ensure you have built it."
    exit 1
}

if ($env:RUNNER_TEMP) {
    $tempDir = $env:RUNNER_TEMP
} else {
    $tempDir = $env:TEMP
}

$stagingDir = Join-Path $tempDir "redscript-archive"
if (Test-Path $stagingDir) {
    Remove-Item -Recurse -Force $stagingDir
}

New-Item -ItemType Directory -Path $stagingDir

Copy-Item -Path "assets/archive/*" -Destination $stagingDir -Recurse
New-Item -ItemType Directory -Path (Join-Path $stagingDir "engine/tools")
Copy-Item -Path "target/release/scc.exe" -Destination (Join-Path $stagingDir "engine/tools/scc.exe")

Compress-Archive -Path (Join-Path $stagingDir '*') -DestinationPath $zipPath

Write-Output "Archive created at $zipPath"
