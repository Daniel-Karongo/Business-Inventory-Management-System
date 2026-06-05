# ================================
# CONFIGURATION
# ================================

$basePath = Get-Location

$searchPhrase = "extends BranchAwareEntity"

$filePatterns = @(
    "*.java",
    "*.kt",
    "*.xml",
    "*.properties",
    "*.yml",
    "*.yaml"
)

$outputFile = Join-Path $basePath "BranchContext_Full_Files.txt"

# ================================
# FIND MATCHING FILES
# ================================

$matchingFiles = @()

foreach ($pattern in $filePatterns) {

    Get-ChildItem `
        -Path $basePath `
        -Recurse `
        -File `
        -Filter $pattern |

    ForEach-Object {

        if (
            Select-String `
                -Path $_.FullName `
                -Pattern $searchPhrase `
                -SimpleMatch `
                -Quiet
        ) {

            $matchingFiles += $_.FullName
        }
    }
}

# Remove duplicates

$matchingFiles =
    $matchingFiles |
    Sort-Object -Unique

# ================================
# BUILD OUTPUT
# ================================

$output = @()

$output += "SEARCH PHRASE: $searchPhrase"
$output += "GENERATED: $(Get-Date)"
$output += ""

foreach ($file in $matchingFiles) {

    Write-Host "Including: $file"

    $output += ""
    $output += "============================================================"
    $output += "FILE: $file"
    $output += "============================================================"
    $output += ""

    $output += "MATCHES:"

    Select-String `
        -Path $file `
        -Pattern $searchPhrase `
        -SimpleMatch |
    ForEach-Object {

        $output += "[Line $($_.LineNumber)] $($_.Line.Trim())"
    }

    $output += ""
    $output += "FULL FILE CONTENT:"
    $output += ""

    $output += Get-Content $file

    $output += ""
    $output += ""
}

# ================================
# WRITE OUTPUT
# ================================

$output |
    Out-File `
        -FilePath $outputFile `
        -Encoding UTF8

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