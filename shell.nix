{
  pkgs ? import <nixpkgs> {}
}:
pkgs.mkShell {
  buildInputs = [
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-test
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-review
    pkgs.jq
  ];
  shellHook = ''
    settings_file=.vscode/settings.json
    set_vscode_setting() {
      jq --arg key "$1" --arg value "$2" '.[$key] = $value' $settings_file > $settings_file.tmp && mv $settings_file.tmp $settings_file
    }
    mkdir -p .vscode
    if [ ! -f $settings_file ]; then
      echo '{}' > $settings_file
    fi
    set_vscode_setting "elmLS.elmPath" "$(which elm)"
    set_vscode_setting "elmLS.elmReviewPath" "$(which elm-review)"
    set_vscode_setting "elmLS.elmFormatPath" "$(which elm-format)"
    set_vscode_setting "elmLS.elmTestPath" "$(which elm-test)"
  '';
}
