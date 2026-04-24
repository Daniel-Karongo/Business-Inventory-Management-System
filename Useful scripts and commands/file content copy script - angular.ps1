# ================================
# CONFIGURATION
# ================================

$basePath = Get-Location
$outputFile = Join-Path $basePath "Selected_Modules_Combined.txt"

$targetFolders = @(
    "package.json",
    "src\environments\environment.ts",
    "src\styles.scss",
    "src\app\app.config.ts",
    "src\app\app.routes.ts",
    "src\app\core\services\auth-error.service.ts",
    "src\app\modules\auth\services\auth.service.ts",
    "src\app\core\services\domain-context.service.ts",
    "src\app\core\services\device.service.ts",
    "src\app\core\services\webauthn.service.ts",
    "src\app\core\guards",
    "src\app\modules\auth\guards\auth.guard.ts",
    "src\app\modules\auth\interceptors\auth.interceptor.ts",
    "src\app\core\interceptors\network-error.interceptor.ts",
    "src\app\core\interceptors\tenant.interceptor.ts"
    "src\app\core\services\date-formats.ts",
    "src\app\core\services\date-utils.ts",
    "src\app\shared\services",
    "src\app\shared\layout\page-shell",
    "src\app\shared\components\confirm-dialog",
    "src\app\shared\components\biometric-manager",
    "src\app\shared\components\biometric-prompt-dialog",
    "src\app\shared\components\overwrite-biometric-dialog",
    "src\app\shared\components\rename-device-dialog",
    "src\app\core\services\biometric-api.service.ts",
    "src\app\core\services\biometric-registration.service.ts",
    "src\app\core\services\device-api.service.ts",
    "src\app\modules\platform"
)

# ================================
# BUILD CONTENT IN MEMORY
# ================================

$builder = New-Object System.Text.StringBuilder

foreach ($folder in $targetFolders) {

    $fullPath = Join-Path $basePath $folder

    if (Test-Path $fullPath) {

        Write-Host "Processing $fullPath"

        Get-ChildItem -Path $fullPath -Recurse -Include *.json, *.ts, *.scss, *.html -File | ForEach-Object {

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