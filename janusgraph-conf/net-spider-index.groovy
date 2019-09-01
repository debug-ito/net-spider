// an init script that returns a Map allows explicit setting of global bindings.
def globals = [:]

// defines a sample LifeCycleHook that prints some output to the Gremlin Server console.
// note that the name of the key in the "global" map is unimportant.
globals << [hook : [
        onStartUp: { ctx ->
            ctx.logger.info("Executed once at startup of Gremlin Server.")
        },
        onShutDown: { ctx ->
            ctx.logger.info("Executed once at shutdown of Gremlin Server.")
        }
] as LifeCycleHook]

println("====================== Start making index");

graph.tx().rollback();
mg = graph.openManagement();
if(mg.getGraphIndex("byNodeId") == null) {
    println("--- get fnode");
    fnode = mg.getVertexLabel("found_node");
    if(fnode == null) {
        fnode = mg.makeVertexLabel("found_node");
    }
    println("--- get node_id");
    node_id = mg.getPropertyKey("@node_id");
    if(node_id == null) {
        node_id = mg.makePropertyKey("@node_id").dataType(String.class).make();
    }
    println("--- make index");
    mg.buildIndex("byNodeId", Vertex.class).addKey(node_id).buildCompositeIndex();
    println("--- commit");
    mg.commit();
}

println("====================== Finish making index");


// define the default TraversalSource to bind queries to - this one will be named "g".
globals << [g : graph.traversal()]
