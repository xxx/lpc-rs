# Gamedriver execution

Language for work performed by the driver; LPC terms are defined in
[the project glossary](doc/glossary.md).

## Language

**Owning task**:
A driver-started action, such as a player command or scheduled callback, whose
game-state changes and those of its nested applies succeed or roll back together.

**Nested apply**:
An apply performed as part of an existing owning task, sharing that task's
success or rollback.
