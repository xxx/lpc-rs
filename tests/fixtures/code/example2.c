// A prototype cycled by the transactional object-space test: created and
// destructed repeatedly within a single transaction. No per-object state
// matters; liveness and identity are asserted in Rust.
void create()
{
}
