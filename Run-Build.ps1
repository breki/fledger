$FledgerVersion = "1.0.2.0";

$ErrorActionPreference = "Stop"

$BuildDir = ".\build"
$BuildPackageDir = "$BuildDir\package"
$BuildPackageBinDir = "$BuildPackageDir\bin"
$BuildPackageConfigDir = "$BuildPackageDir\config"
$BuildPackageDbDir = "$BuildPackageDir\db"
$BuildPackageDocsDir = "$BuildPackageDir\docs"

$Configuration = "Release";

function Log($Message)
{
    Write-Output "------------------------------"
    Write-Output $Message
    Write-Output "--------"
}

function FailOnError()
{
    if ($LastExitCode -ne 0)
    {
        Write-Output "BUILD SCRIPT FAILED"
        exit $LastExitCode
    }
}

function Get-Tree($Path, $Include = '*')
{
    @(Get-Item $Path -Include $Include -Force) +
            (Get-ChildItem $Path -Recurse -Include $Include -Force) |
            Sort-Object pspath -Descending -unique
}

# A special function to really remove the whole directory structure,
# including the empty subdirectories.
# Taken from https://stackoverflow.com/a/11520550/55408
function Remove-Tree($Path, $Include = '*')
{
    if (Test-Path -Path $Path)
    {
        Get-Tree $Path $Include | Remove-Item -force -recurse
    }
}

function CleanOldBuildArtifacts()
{
    Log "Cleaning previous build's artifacts..."
    dotnet clean --configuration $Configuration --verbosity minimal --nologo
    FailOnError

    Remove-Tree fledger\bin\$Configuration
    FailOnError

    Remove-Tree fledger.Tests\bin\$Configuration
    FailOnError

    Remove-Tree fledger.tool\bin\$Configuration
    FailOnError

    Remove-Tree $BuildDir
    FailOnError
}

function BuildSolution()
{
    Log "Compiling..."
    dotnet build --configuration $Configuration --no-incremental
    FailOnError
}

function Test()
{
    # note that the coverage collecting is commented out because the latest 
    # dotcover command does not recognize the "test" command for some reason
    
    Log "Running tests..."
#    dotnet dotcover test --configuration $Configuration `
#        "--dcOutput=$BuildDir\fledger-coverage-report.dcvr" --no-build --nologo
    
    dotnet test --configuration $Configuration --no-build --nologo
    
    FailOnError
#    dotnet dotcover report dotcover-report-config.xml
#    FailOnError
}

function CompileBinariesFinal()
{
    Log "Building the final package..."
    dotnet build fledger.tool --configuration $Configuration --runtime win-x64 `
       "/p:AssemblyVersion=$FledgerVersion" `
       "/p:FileVersion=$FledgerVersion" `
       --no-self-contained --no-incremental
    FailOnError
}

function CopyBinariesToPackageArea()
{
    Log "Copying the deliverables to the build package directory..."
    New-Item $BuildPackageBinDir -ItemType directory | Out-Null
    FailOnError

    Copy-Item -Path fledger.tool\bin\$Configuration\net6.0\win-x64\* `
	  -Destination $BuildPackageBinDir -Recurse -Force
    FailOnError
}

function ZipDeploymentPackage()
{
    Log "Zipping the delivery package..."
    Compress-Archive -Path $BuildPackageDir\* `
        -Destination $BuildPackageDir\fledger-$FledgerVersion.zip -Force
    FailOnError
}

function CreateDeploymentPackage()
{
    CopyBinariesToPackageArea
    ZipDeploymentPackage
}

function BuildOnBitbucket()
{
    $Env:REPORTS_PATH = "./test-reports/build_$Env:BITBUCKET_BUILD_NUMBER"
    dotnet add fledger.Tests package JUnitTestLogger --version 1.1.0
    FailOnError

    Log "Compiling..."
    dotnet build --configuration $Configuration --no-incremental
    FailOnError

    Log "Running tests..."
    dotnet test --configuration $Configuration --filter "Category!=Db" `
        --test-adapter-path:. `
        --logger: "junit;LogFilePath=$Env:REPORTS_PATH/junit.xml" `
        --no-build --nologo
    FailOnError
}

Log "Restoring dotnet tools..."
dotnet tool restore
FailOnError

CleanOldBuildArtifacts

BuildSolution
Test
CompileBinariesFinal
CreateDeploymentPackage
