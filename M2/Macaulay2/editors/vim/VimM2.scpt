on run argv
        set folderName to item 1 of argv
        set scrName to item 2 of argv

        tell application "Terminal"
                activate
                set cmdToRun to "cd " & quoted form of (folderName as strin
                tell application "System Events"
                        keystroke "n" using {command down}
                end tell
                do script cmdToRun in first window of application "Terminal
        end tell
end run

