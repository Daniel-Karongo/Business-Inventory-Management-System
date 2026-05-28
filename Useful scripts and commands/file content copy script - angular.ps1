# ================================
# CONFIGURATION
# ================================

$basePath = Get-Location
$outputFile = Join-Path $basePath "Selected_Modules_Combined.txt"

$targetFolders = @(
    "src\app\app.routes.ts",
    "src\app\modules\tenant\tenant.routes.ts",
    "src\app\modules\tenant\navigation\sidebar",
    "src\app\modules\tenant\content\stock\models",
    "src\app\modules\tenant\content\stock\inventory",
    "src\app\modules\tenant\content\stock\products"
)

# ================================
# CLEAR OUTPUT FILE
# ================================

"" | Set-Content $outputFile

# ================================
# PROCESS FILES
# ================================

foreach ($folder in $targetFolders) {

    $fullPath = Join-Path $basePath $folder

    if (Test-Path $fullPath) {

        Write-Host "Processing $fullPath"

        Get-ChildItem `
            -Path $fullPath `
            -Recurse `
            -Include *.json, *.ts, *.scss, *.html `
            -File | ForEach-Object {

                Add-Content $outputFile ""
                Add-Content $outputFile "============================================================"
                Add-Content $outputFile "FILE: $($_.FullName)"
                Add-Content $outputFile "============================================================"
                Add-Content $outputFile ""

                Get-Content $_.FullName | Add-Content $outputFile
        }
    }
}

# ================================
# FINISH
# ================================

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