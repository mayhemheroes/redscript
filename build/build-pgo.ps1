# This script downloads Codeware, a script cache, and runs REDscript multiple times to
# generate a PGO profile. It then merges the profiles and builds REDscript with the PGO profile.

$codewareVersion = "1.14.1"
$codewareUrl = "https://github.com/psiberx/cp2077-codeware/releases/download/v$codewareVersion/Codeware-$codewareVersion.zip"
$cacheUrl = "https://file.io/01QAMSSG91um"

$codewarePath = Join-Path $env:TEMP "codeware"
$cacheDir = Join-Path $env:TEMP "redscript-cache"
$cachePath = Join-Path $cacheDir "final.redscripts"
$pgoPath = Join-Path $env:TEMP "redscript-pgo"

if (-Not (Test-Path -Path $codewarePath)) {
    $tempDir = New-Item -ItemType Directory -Path $codewarePath -Force
    $outFile = Join-Path $tempDir.FullName (New-Object System.Uri($codewareUrl)).Segments[-1]
    Invoke-WebRequest -Uri $codewareUrl -OutFile $outFile
    Expand-Archive -Path $zipFilePath -DestinationPath $tempDir.FullName -Force
} else {
    Write-Host "The temporary directory already exists. Skipping download."
}

if (-Not (Test-Path -Path $cacheDir)) {
    $tempDir = New-Item -ItemType Directory -Path $cacheDir -Force
    Invoke-WebRequest -Uri $cacheUrl -OutFile $cachePath
} else {
    Write-Host "The cache directory already exists. Skipping download."
}

if (Test-Path -Path $pgoPath) {
    Remove-Item -Path $pgoPath -Recurse -Force
}

$cargoDir = rustup which cargo
$defaultToolchain = rustup show active-toolchain
$defaultToolchain -match 'stable-(.+?) \(default\)'
$triplet = $matches[1]
$llvmProfdata = "$cargoDir\..\..\lib\rustlib\$triplet\bin\llvm-profdata.exe"

rustup component add llvm-tools-preview

& { $env:RUSTFLAGS = "-Cprofile-generate=$pgoPath"; cargo build --release --target=$triplet }

foreach ($i in 1..10) {
    & ".\target\$triplet\release\scc.exe" -compile "$codewarePath\red4ext\plugins\Codeware\Scripts" $cachePath
}
& $llvmProfdata merge -o "$pgoPath\merged.profdata" $pgoPath

& { $env:RUSTFLAGS = "-Cprofile-use=$pgoPath\merged.profdata"; cargo build --release --target=$triplet }
