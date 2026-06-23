/* =========================================================================
   app.js — count-up stat counters + celebratory confetti + loading overlay +
   the multi-frame map "kick" (re-fit a Leaflet map that initialised hidden).
   Ported from the Mosquito Pulse v2 chrome; behaviour identical.
   ========================================================================= */

// When a tab becomes visible, nudge a resize so widgets that rendered while the
// tab was hidden (Leaflet maps, plotly charts) re-fit to their real size — the
// classic "0-sized widget in a hidden bootstrap tab" fix.
document.addEventListener("shown.bs.tab", function () {
  setTimeout(function () { window.dispatchEvent(new Event("resize")); }, 60);
});

// ---- animated count-up for the hero stat band ----------------------------
function animateCount(el) {
  if (el.dataset.animated === "1") return;
  el.dataset.animated = "1";
  // A freshly-rendered hero counter means a site just finished loading — the
  // most reliable signal to dismiss the loading overlay.
  if (typeof smtLoadDone === "function") smtLoadDone();
  const target = parseFloat(el.getAttribute("data-target")) || 0;
  const suffix = el.dataset.suffix || "";
  const isFloat = !Number.isInteger(target);
  const fmt = (v) => (isFloat ? v.toFixed(1) : Math.round(v).toLocaleString()) + suffix;
  if (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) {
    el.textContent = fmt(target); return;
  }
  const dur = 900;
  const start = performance.now();
  function tick(now) {
    const t = Math.min(1, (now - start) / dur);
    const eased = 1 - Math.pow(1 - t, 3); // easeOutCubic
    el.textContent = fmt(target * eased);
    if (t < 1) requestAnimationFrame(tick);
    else el.textContent = fmt(target);
  }
  requestAnimationFrame(tick);
}
function runCounters() {
  document.querySelectorAll(".count-up").forEach(animateCount);
}
const heroObserver = new MutationObserver(() => runCounters());
document.addEventListener("DOMContentLoaded", function () {
  heroObserver.observe(document.body, { childList: true, subtree: true });
  runCounters();
});

// ---- confetti on a standout (a top EPT site, etc.) -----------------------
function rodentConfetti(big) {
  if (typeof confetti !== "function") return;
  // Riffle & Teal palette (teal / aqua / green / amber / ink).
  const colors = ["#0e8f9c", "#2bb7c4", "#3f9e6e", "#e08a2b", "#102a33"];
  const burst = (opts) => confetti(Object.assign({ colors, disableForReducedMotion: true }, opts));
  burst({ particleCount: big ? 140 : 70, spread: big ? 100 : 70, origin: { y: 0.3 }, startVelocity: 42 });
  if (big) {
    setTimeout(() => burst({ particleCount: 80, angle: 60, spread: 70, origin: { x: 0 } }), 180);
    setTimeout(() => burst({ particleCount: 80, angle: 120, spread: 70, origin: { x: 1 } }), 320);
  }
  mascotCheer(big);
}
function mascotCheer(big) {
  try {
    if (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) return;
    var src = document.querySelector("#loadOverlay .mascot");
    if (!src) return;
    var wrap = document.createElement("div");
    wrap.className = "mascot-cheer";
    wrap.appendChild(src.cloneNode(true));
    document.body.appendChild(wrap);
    setTimeout(function () { if (wrap.parentNode) wrap.parentNode.removeChild(wrap); }, 1700);
  } catch (e) {}
}

// ---- first-visit: the splash mascot waves hello once (localStorage-gated) ----
document.addEventListener("DOMContentLoaded", function () {
  try {
    if (localStorage.getItem("invMascotSeen") === "1") return;
    var g = document.querySelector(".splash-guide");
    if (g) {
      g.classList.add("wave");
      localStorage.setItem("invMascotSeen", "1");
      setTimeout(function () { g.classList.remove("wave"); }, 3300);
    }
  } catch (e) {}
});

// ---- loading overlay (opaque, indeterminate) -----------------------------
var smtSafetyTimer = null;
function smtLoadStart(label) {
  var ov = document.getElementById("loadOverlay");
  if (!ov) return;
  var siteText = label || "";
  if (!siteText) {
    var sel = document.getElementById("site");
    if (sel && sel.options && sel.selectedIndex >= 0) siteText = sel.options[sel.selectedIndex].text;
  }
  var siteEl = document.getElementById("loadSite");
  if (siteEl) siteEl.textContent = siteText;
  ov.style.display = "flex";
  if (navigator.vibrate) { try { navigator.vibrate(12); } catch (e) {} }
  clearTimeout(smtSafetyTimer);
  smtSafetyTimer = setTimeout(function () {
    var note = document.querySelector(".load-note");
    if (note) note.textContent = "Still working — a large site can take a moment. You can close this and try again.";
    setTimeout(smtLoadDone, 5000);
  }, 90000);
}
function smtLoadDone() {
  clearTimeout(smtSafetyTimer);
  var ov = document.getElementById("loadOverlay");
  if (ov) ov.style.display = "none";
}

// ---- dismiss any open info popover (click-outside + Esc) -----------------
function smtClosePopovers() {
  document.querySelectorAll(".popover").forEach(function (pop) {
    var trig = pop.id ? document.querySelector('[aria-describedby="' + pop.id + '"]') : null;
    if (trig && window.bootstrap && bootstrap.Popover) {
      var inst = bootstrap.Popover.getInstance(trig);
      if (inst) { inst.hide(); return; }
    }
    pop.remove();
  });
}
document.addEventListener("click", function (e) {
  if (e.target.closest(".popover") || e.target.closest(".info-dot") ||
      e.target.closest("bslib-popover")) return;
  if (document.querySelector(".popover")) smtClosePopovers();
});
document.addEventListener("keydown", function (e) {
  if (e.key === "Escape") smtClosePopovers();
});

// ---- Shiny custom message handlers ---------------------------------------
document.addEventListener("DOMContentLoaded", function () {
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("countUp", function () {
      setTimeout(runCounters, 60);
    });
    Shiny.addCustomMessageHandler("confetti", function (msg) {
      rodentConfetti(msg && msg.big);
    });
    Shiny.addCustomMessageHandler("loadDone", function () { smtLoadDone(); });
    Shiny.addCustomMessageHandler("smtLoadStart", function (msg) {
      smtLoadStart(msg && msg.label);
    });
    // A Leaflet map that initialised inside a hidden container (the within-site
    // Map tab, or the picker map re-shown after "change site") can paint blank
    // until it recomputes its size. Dispatching 'resize' over several frames
    // makes every Leaflet map invalidateSize. The server kicks this after
    // re-showing the splash and on relevant tab shows.
    Shiny.addCustomMessageHandler("kickMaps", function () {
      var kick = function () {
        try { window.dispatchEvent(new Event("resize")); } catch (e) {}
        try {
          if (window.HTMLWidgets && HTMLWidgets.findAll) {
            HTMLWidgets.findAll(".leaflet").forEach(function (w) {
              if (w.getMap && w.getMap()) w.getMap().invalidateSize();
            });
          }
        } catch (e) {}
      };
      requestAnimationFrame(kick);
      [80, 250, 500, 900].forEach(function (t) { setTimeout(kick, t); });
    });
  }
});
