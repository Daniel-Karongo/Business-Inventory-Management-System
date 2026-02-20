# ================================
# CONFIGURATION
# ================================
$basePath = Get-Location
$outputFile = Join-Path $basePath "Selected_Modules_Combined.java"

$targetFolders = @(
    "modules\stock\inventory",
    "modules\stock\product",
    "modules\finance\sales",
    "modules\finance\payment",
    "modules\finance\accounting"
)

# ================================
# BUILD CONTENT IN MEMORY
# ================================
$builder = New-Object System.Text.StringBuilder

foreach ($folder in $targetFolders) {
    $fullPath = Join-Path $basePath $folder

    if (Test-Path $fullPath) {
        Write-Host "Processing $fullPath"

        Get-ChildItem -Path $fullPath -Recurse -Filter *.java | ForEach-Object {

            $builder.AppendLine("") | Out-Null
            $builder.AppendLine("============================================================") | Out-Null
            $builder.AppendLine("FILE: $($_.FullName)") | Out-Null
            $builder.AppendLine("============================================================") | Out-Null
            $builder.AppendLine("") | Out-Null

            Get-Content $_.FullName | ForEach-Object {
                $builder.AppendLine($_) | Out-Null
            }
        }
    }
}

# ================================
# WRITE FILE ONCE (No Lock Issues)
# ================================
[System.IO.File]::WriteAllText($outputFile, $builder.ToString())

Write-Host "Combined file created at: $outputFile"

# ================================
# OPEN IN EDITOR
# ================================
if (Get-Command code -ErrorAction SilentlyContinue) {
    code $outputFile
}
else {
    notepad $outputFile
}