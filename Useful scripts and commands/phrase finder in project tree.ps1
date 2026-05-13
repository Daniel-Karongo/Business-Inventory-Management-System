# ================================
# CONFIGURATION
# ================================

$basePath = Get-Location
$searchPhrase = "BranchContext"

$filePatterns = @(
    "*.java",
    "*.kt",
    "*.xml",
    "*.properties",
    "*.yml",
    "*.yaml"
)

$outputFile = Join-Path $basePath "BranchContext_Context_Search.txt"

$builder = New-Object System.Text.StringBuilder

# ================================
# SEARCH
# ================================

foreach ($pattern in $filePatterns) {

    Get-ChildItem `
        -Path $basePath `
        -Recurse `
        -File `
        -Filter $pattern |
    ForEach-Object {

        $matches = Select-String `
            -Path $_.FullName `
            -Pattern $searchPhrase `
            -SimpleMatch `
            -Context 2,2

        if ($matches) {

            $builder.AppendLine("") | Out-Null
            $builder.AppendLine("===================================================") | Out-Null
            $builder.AppendLine("FILE: $($_.FullName)") | Out-Null
            $builder.AppendLine("===================================================") | Out-Null
            $builder.AppendLine("") | Out-Null

            foreach ($match in $matches) {

                $builder.AppendLine("Line: $($match.LineNumber)") | Out-Null
                $builder.AppendLine("-------------------------------------------") | Out-Null

                foreach ($ctx in $match.Context.PreContext) {
                    $builder.AppendLine($ctx) | Out-Null
                }

                $builder.AppendLine(">>> " + $match.Line) | Out-Null

                foreach ($ctx in $match.Context.PostContext) {
                    $builder.AppendLine($ctx) | Out-Null
                }

                $builder.AppendLine("") | Out-Null
            }
        }
    }
}

# ================================
# WRITE FILE
# ================================

[System.IO.File]::WriteAllText(
    $outputFile,
    $builder.ToString()
)

Write-Host ""
Write-Host "Search complete."
Write-Host "Results written to:"
Write-Host $outputFile

# ================================
# OPEN IN VS CODE
# ================================

if (Get-Command code -ErrorAction SilentlyContinue) {
    code $outputFile
}