*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

INTERFACE types.
  TYPES:
    ty_head_type TYPE c LENGTH 1,
    BEGIN OF ty_remote_settings,
      offline         TYPE zif_abapgit_persistence=>ty_repo-offline,
      url             TYPE zif_abapgit_persistence=>ty_repo-url,
      branch          TYPE zif_abapgit_git_definitions=>ty_git_branch-name,
      tag             TYPE zif_abapgit_git_definitions=>ty_git_tag-name,
      commit          TYPE zif_abapgit_git_definitions=>ty_commit-sha1,
      pull_request    TYPE string,
      head_type       TYPE ty_head_type,
      switched_origin TYPE zif_abapgit_persistence=>ty_repo-switched_origin,
    END OF ty_remote_settings.
ENDINTERFACE.


INTERFACE const.

  CONSTANTS:
    BEGIN OF repo_type,
      online  TYPE string VALUE 'Online Repository',
      offline TYPE string VALUE 'Offline Repository',
    END OF repo_type.
  CONSTANTS:
    BEGIN OF head_types,
      branch       TYPE types=>ty_head_type VALUE 'B',
      tag          TYPE types=>ty_head_type VALUE 'T',
      commit       TYPE types=>ty_head_type VALUE 'C',
      pull_request TYPE types=>ty_head_type VALUE 'P',
    END OF head_types.
  CONSTANTS:
    BEGIN OF id,
      general      TYPE string VALUE 'general',
      repo_type    TYPE string VALUE 'repo_type',
      offline      TYPE string VALUE 'offline',
      url          TYPE string VALUE 'url',
      head_group   TYPE string VALUE 'head_group',
      branch       TYPE string VALUE 'branch',
      tag          TYPE string VALUE 'tag',
      commit       TYPE string VALUE 'commit',
      pull_request TYPE string VALUE 'pull_request',
      head_type    TYPE string VALUE 'head_type',
    END OF id.
  CONSTANTS:
    BEGIN OF event,
      save                TYPE string VALUE 'save',
      switch              TYPE string VALUE 'switch',
      choose_url          TYPE string VALUE 'choose_url',
      choose_branch       TYPE string VALUE 'choose_branch',
      choose_tag          TYPE string VALUE 'choose_tag',
      choose_commit       TYPE string VALUE 'choose_commit',
      choose_pull_request TYPE string VALUE 'choose_pull_request',
      change_head_type    TYPE string VALUE 'change_head_type',
    END OF event.

ENDINTERFACE.
