# ================================
# CONFIGURATION
# ================================

$basePath = Get-Location
$outputFile = Join-Path $basePath "Selected_Modules_Combined.txt"

$targetFolders = @(
    "styles.scss",
    "app\app.config.ts",
    "app\app.routes.ts",
    "app\core\layout",
    "app\core\navigation",
    "app\modules\dashboard",
    "app\modules\accounts",
    "app\shared\widgets"
)

# ================================
# BUILD CONTENT IN MEMORY
# ================================

$builder = New-Object System.Text.StringBuilder

foreach ($folder in $targetFolders) {

    $fullPath = Join-Path $basePath $folder

    if (Test-Path $fullPath) {

        Write-Host "Processing $fullPath"

        Get-ChildItem -Path $fullPath -Recurse -Include *.ts, *.scss, *.html -File | ForEach-Object {

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
# WRITE FILE ONCE
# ================================

[System.IO.File]::WriteAllText($outputFile, $builder.ToString())

Write-Host "Combined file created at: $outputFile"

# ================================
# OPEN IN EDITOR
# ================================

if (Get-Command code -ErrorAction SilentlyContinue) {
    code $outputFile
} else {
    notepad $outputFile
}