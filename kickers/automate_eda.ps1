$project_root = $PSScriptRoot

$log_dir = "$project_root\logs"

Get-ChildItem -Path $log_dir -Filter "*.txt" |
Where-Object { $_.CreationTime -lt (Get-Date).AddDays(-30) } |
Remove-Item -Force

$timestamp = Get-Date -Format "yyyy-MM-dd-HHmm"
$log_file = Join-Path $log_dir "run_$($timestamp).txt"
Start-Transcript -Path $log_file -Append

try {

	# Defining the window of script automation
	$current_month = (Get-Date).Month
	$current_day = (Get-Date).Day
	$active_months = 8, 9, 10, 11, 12, 1
	$update_day = 7
	$in_season = $active_months -contains $current_month

	if (-not $in_season) {

		if ($current_day -gt $update_day) {
			Write-Host "Off-season: Monthly update already completed. Exiting" -ForegroundColor Gray
			return
		}
		Write-Host "Off-season: Running monthly maintenance update..." -ForegroundColor Yellow

	}
	else {
		Write-Host "In-season: Running weekly kicker analysis..." -ForegroundColor Cyan
	}

	$etl_script = "$project_root\etl\etl.R"
	$etl_input_file = "$project_root\etl\variables\variables.csv"

	$cleaning_script = "$project_root\cleaning\cleaning.R"
	
	$pp_script = "$project_root\pp\pp.R"

	$pbp_plotting = "$project_root/eda/plotting/pbp.csv"
	$stats_plotting = "$project_root/eda/plotting/stats.csv"

	$report_name = "EDA_$(Get-Date -Format 'yyyy-MM-dd').html"
	$output_dir = "$project_root\eda"

  $scripts = @(
    @{ Name = "ETL"; Path = $etl_script; Args = $etl_input_file },
    @{ Name = "Cleaning"; Path = $cleaning_script; Args = $null },
    @{ Name = "PP"; Path = $pp_script; Args = $null }
  )
  foreach ($s in $scripts) {
    if (Test-Path $s.Path) {
      Write-Host "Executing the $($s.Name) Script" -ForegroundColor Cyan
      if ($s.Args) { Rscript "$($s.Path)" "$($s.Args)" } else { Rscript "$($s.Path)" }
      
      if ($LASTEXITCODE -eq 0) { 
        Write-Host "$($s.Name) Script was run successfully." -ForegroundColor Green 
      }
      else { 
        Write-Warning "$($s.Name) Script exited with error code: $LASTEXITCODE" 
      }
    } else {
        Write-Error "Could not find script at: $($s.Path)"
    }
  }

	# Generating an EDA File
	Write-Host "Generating the EDA report..." -ForegroundColor Cyan
	
	Push-Location $output_dir
	
	try {

		quarto render "eda.qmd" `
		  --output "$report_name" `
		  -P "pbp_plotting:$pbp_plotting" `
		  -P "stats_plotting:$stats_plotting"
		  
		if ($LASTEXITCODE -eq 0) {
			Write-Host "Success: EDA report generated successfully." -ForegroundColor Green

			if ([Environment]::UserInteractive) {
				$response = Read-Host -Prompt "Would you like to view the EDA report? (Y/N)"
				if ($response -eq "Y") {
					Write-Host "Launching EDA report..." -ForegroundColor Yellow
					Start-Process "$report_name"
				}
				else {
					Write-Host "Report rendered and saved to $output_dir\$report_name" -ForegroundColor Gray
				}
			}
		}
		else {
			Write-Error "Failure: Quarto encountered an error during rendering. Check your qmd code logic."
		}
	}
	catch {
		Write-Host "Critical Error: Could not execute Quarto $( $_.Exception.Message )" -ForegroundColor Red
	}
	finally {
	  Pop-Location 
	}
}
finally {
	Stop-Transcript
}