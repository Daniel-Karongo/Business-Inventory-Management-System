# ================================
# CONFIGURATION
# ================================

$basePath = Get-Location

# Phrase to search for
$searchPhrase = "BranchContext"

# File types to scan
$filePatterns = @(
    "*.java",
    "*.kt",
    "*.xml",
    "*.properties",
    "*.yml",
    "*.yaml"
)

# Output file
$outputFile = Join-Path $basePath "BranchContext_Full_Files.txt"

# ================================
# FIND MATCHING FILES
# ================================

$matchingFiles = New-Object System.Collections.Generic.HashSet[string]

foreach ($pattern in $filePatterns) {

    Get-ChildItem `
        -Path $basePath `
        -Recurse `
        -File `
        -Filter $pattern |
    ForEach-Object {

        $match = Select-String `
            -Path $_.FullName `
            -Pattern $searchPhrase `
            -SimpleMatch `
            -Quiet

        if ($match) {
            $matchingFiles.Add($_.FullName) | Out-Null
        }
    }
}

# ================================
# BUILD OUTPUT
# ================================

$builder = New-Object System.Text.StringBuilder

$builder.AppendLine("SEARCH PHRASE: $searchPhrase") | Out-Null
$builder.AppendLine("GENERATED: $(Get-Date)") | Out-Null
$builder.AppendLine("") | Out-Null

foreach ($file in $matchingFiles) {

    Write-Host "Including: $file"

    $builder.AppendLine("") | Out-Null
    $builder.AppendLine("============================================================") | Out-Null
    $builder.AppendLine("FILE: $file") | Out-Null
    $builder.AppendLine("============================================================") | Out-Null
    $builder.AppendLine("") | Out-Null

    # Optional: include matching lines first
    $matches = Select-String `
        -Path $file `
        -Pattern $searchPhrase `
        -SimpleMatch

    $builder.AppendLine("MATCHES:") | Out-Null

    foreach ($m in $matches) {
        $builder.AppendLine(
            "[Line $($m.LineNumber)] $($m.Line.Trim())"
        ) | Out-Null
    }

    $builder.AppendLine("") | Out-Null
    $builder.AppendLine("FULL FILE CONTENT:") | Out-Null
    $builder.AppendLine("") | Out-Null

    Get-Content $file | ForEach-Object {
        $builder.AppendLine($_) | Out-Null
    }

    $builder.AppendLine("") | Out-Null
    $builder.AppendLine("") | Out-Null
}

# ================================
# WRITE OUTPUT FILE
# ================================

[System.IO.File]::WriteAllText(
    $outputFile,
    $builder.ToString()
)

Write-Host ""
Write-Host "Done."
Write-Host "Output file:"
Write-Host $outputFile

# ================================
# OPEN IN VS CODE
# ================================

if (Get-Command code -ErrorAction SilentlyContinue) {
    code $outputFile
}