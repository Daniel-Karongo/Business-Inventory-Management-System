# ================================
# CONFIGURATION
# ================================

$basePath = Get-Location
$outputFile = Join-Path $basePath "Selected_Modules_Combined.txt"

$targetFolders = @(
    "java/com/IntegrityTechnologies/business_manager/modules/stock/inventory/controller",
    "java/com/IntegrityTechnologies/business_manager/modules/stock/inventory/dto",
    "java/com/IntegrityTechnologies/business_manager/modules/stock/product/parent/controller",
    "java/com/IntegrityTechnologies/business_manager/modules/stock/product/parent/dto",
    "java/com/IntegrityTechnologies/business_manager/modules/stock/product/variant/pricing/controller",
    "java/com/IntegrityTechnologies/business_manager/modules/stock/product/variant/pricing/dto",
    "java/com/IntegrityTechnologies/business_manager/modules/stock/product/variant/base/controller",
    "java/com/IntegrityTechnologies/business_manager/modules/stock/product/variant/base/dto",
    "java/com/IntegrityTechnologies/business_manager/modules/stock/product/variant/packaging/controller",
    "java/com/IntegrityTechnologies/business_manager/modules/stock/product/variant/packaging/dto"
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
            -Include *.java, *.jrxml, *.xml, *.properties, *.yml `
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