// Simple force-directed layout (Fruchterman-Reingold style)
// No dependencies — just fetch + iterate

export const loadMiserablesImpl = (onError) => (onSuccess) => () => {
  fetch("./data/miserables.json")
    .then(r => r.json())
    .then(data => {
      const result = forceLayout(data, 900, 320);
      onSuccess(result)();
    })
    .catch(e => {
      onError(e.message)();
    });
};

function forceLayout(data, width, height) {
  const nodes = data.nodes.map(n => ({
    id: n.id,
    group: n.group,
    x: width / 2 + (Math.random() - 0.5) * width * 0.8,
    y: height / 2 + (Math.random() - 0.5) * height * 0.8,
    vx: 0,
    vy: 0
  }));

  const nodeMap = {};
  nodes.forEach(n => { nodeMap[n.id] = n; });

  const links = data.links
    .filter(l => nodeMap[l.source] && nodeMap[l.target])
    .map(l => ({ source: l.source, target: l.target }));

  // Group centers — spread groups in a circle
  const groups = [...new Set(nodes.map(n => n.group))].sort((a, b) => a - b);
  const groupCenters = {};
  groups.forEach((g, i) => {
    const angle = (2 * Math.PI * i) / groups.length - Math.PI / 2;
    const r = Math.min(width, height) * 0.3;
    groupCenters[g] = {
      x: width / 2 + r * Math.cos(angle),
      y: height / 2 + r * Math.sin(angle)
    };
  });

  // Initialize positions near group centers
  nodes.forEach(n => {
    const gc = groupCenters[n.group];
    n.x = gc.x + (Math.random() - 0.5) * 60;
    n.y = gc.y + (Math.random() - 0.5) * 60;
  });

  // Iterate force simulation
  const iterations = 300;
  const margin = 30;

  for (let iter = 0; iter < iterations; iter++) {
    const alpha = 1 - iter / iterations;
    const repulsion = 800 * alpha;
    const attraction = 0.05 * alpha;
    const groupPull = 0.02 * alpha;

    // Reset velocities
    nodes.forEach(n => { n.vx = 0; n.vy = 0; });

    // Repulsion between all pairs (O(n^2) but n=77 so fine)
    for (let i = 0; i < nodes.length; i++) {
      for (let j = i + 1; j < nodes.length; j++) {
        let dx = nodes[i].x - nodes[j].x;
        let dy = nodes[i].y - nodes[j].y;
        let d2 = dx * dx + dy * dy;
        if (d2 < 1) d2 = 1;
        const f = repulsion / d2;
        const fx = dx * f;
        const fy = dy * f;
        nodes[i].vx += fx;
        nodes[i].vy += fy;
        nodes[j].vx -= fx;
        nodes[j].vy -= fy;
      }
    }

    // Attraction along edges
    links.forEach(l => {
      const s = nodeMap[l.source];
      const t = nodeMap[l.target];
      const dx = t.x - s.x;
      const dy = t.y - s.y;
      const d = Math.sqrt(dx * dx + dy * dy);
      if (d > 0) {
        const f = attraction * d;
        const fx = dx / d * f;
        const fy = dy / d * f;
        s.vx += fx;
        s.vy += fy;
        t.vx -= fx;
        t.vy -= fy;
      }
    });

    // Group gravity
    nodes.forEach(n => {
      const gc = groupCenters[n.group];
      n.vx += (gc.x - n.x) * groupPull;
      n.vy += (gc.y - n.y) * groupPull;
    });

    // Apply velocities with damping
    nodes.forEach(n => {
      n.x += n.vx * 0.5;
      n.y += n.vy * 0.5;
      // Clamp to bounds
      n.x = Math.max(margin, Math.min(width - margin, n.x));
      n.y = Math.max(margin, Math.min(height - margin, n.y));
    });
  }

  // Return as arrays for PureScript
  return {
    nodes: nodes.map(n => ({ id: n.id, x: n.x, y: n.y })),
    edges: links.map(l => ({ source: l.source, target: l.target }))
  };
}
