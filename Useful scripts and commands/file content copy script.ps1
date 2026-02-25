# ================================
# CONFIGURATION
# ================================
$basePath = Get-Location
$outputFile = Join-Path $basePath "Selected_Modules_Combined.java"

$targetFolders = @(
    "java\com\IntegrityTechnologies\business_manager\modules\finance\accounting",
    "java\com\IntegrityTechnologies\business_manager\modules\finance\budgeting",
    "java\com\IntegrityTechnologies\business_manager\modules\dashboard",
    "java\com\IntegrityTechnologies\business_manager\modules\communication\reports",
    "resources\reports\accounting"
)

# ================================
# BUILD CONTENT IN MEMORY
# ================================
$builder = New-Object System.Text.StringBuilder

foreach ($folder in $targetFolders) {
    $fullPath = Join-Path $basePath $folder

    if (Test-Path $fullPath) {
        Write-Host "Processing $fullPath"
        
        Get-ChildItem -Path $fullPath -Recurse -Include *.java, *.jrxml, *.xml, *.properties, *.yml -File | ForEach-Object {

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