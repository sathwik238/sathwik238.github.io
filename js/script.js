'use strict';

/* ===================================================
   PARTICLE CANVAS
=================================================== */
function initParticles() {
  const canvas = document.getElementById('particleCanvas');
  if (!canvas) return;
  const ctx = canvas.getContext('2d');

  let width, height, particles, animId;

  const PARTICLE_COUNT_DIVISOR = 9;  // particles = width / this
  const MAX_PARTICLES = 130;
  const MIN_PARTICLES = 60;
  const CONNECT_DIST = 130;
  const SPEED = 0.4;

  function resize() {
    width  = canvas.width  = canvas.offsetWidth;
    height = canvas.height = canvas.offsetHeight;
  }

  function randomBetween(a, b) {
    return a + Math.random() * (b - a);
  }

  function createParticle() {
    return {
      x:  randomBetween(0, width),
      y:  randomBetween(0, height),
      vx: randomBetween(-SPEED, SPEED) || SPEED * 0.5,
      vy: randomBetween(-SPEED, SPEED) || SPEED * 0.5,
      r:  randomBetween(1.5, 3),
    };
  }

  function spawnParticles() {
    const count = Math.min(MAX_PARTICLES, Math.max(MIN_PARTICLES, Math.floor(width / PARTICLE_COUNT_DIVISOR)));
    particles = Array.from({ length: count }, createParticle);
  }

  function update(p) {
    p.x += p.vx;
    p.y += p.vy;
    if (p.x < 0 || p.x > width)  p.vx *= -1;
    if (p.y < 0 || p.y > height) p.vy *= -1;
  }

  function drawParticle(p) {
    ctx.beginPath();
    ctx.arc(p.x, p.y, p.r, 0, Math.PI * 2);
    ctx.fillStyle = 'rgba(0, 245, 255, 0.65)';
    ctx.fill();
  }

  function drawConnections() {
    for (let i = 0; i < particles.length; i++) {
      for (let j = i + 1; j < particles.length; j++) {
        const dx = particles[i].x - particles[j].x;
        const dy = particles[i].y - particles[j].y;
        const dist = Math.sqrt(dx * dx + dy * dy);
        if (dist < CONNECT_DIST) {
          const alpha = 1 - dist / CONNECT_DIST;
          // Blend cyan to purple based on normalized distance
          const t = dist / CONNECT_DIST;
          const r = Math.round(0   + t * 123);
          const g = Math.round(245 + t * (47  - 245));
          const b = Math.round(255 + t * (255 - 255));
          ctx.beginPath();
          ctx.moveTo(particles[i].x, particles[i].y);
          ctx.lineTo(particles[j].x, particles[j].y);
          ctx.strokeStyle = `rgba(${r}, ${g}, ${b}, ${alpha * 0.4})`;
          ctx.lineWidth = 1;
          ctx.stroke();
        }
      }
    }
  }

  function animate() {
    ctx.clearRect(0, 0, width, height);
    particles.forEach(update);
    drawConnections();
    particles.forEach(drawParticle);
    animId = requestAnimationFrame(animate);
  }

  function init() {
    cancelAnimationFrame(animId);
    resize();
    spawnParticles();
    animate();
  }

  // Debounced resize
  let resizeTimer;
  window.addEventListener('resize', () => {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(init, 200);
  });

  init();
}

/* ===================================================
   TYPEWRITER EFFECT
=================================================== */
function initTypewriter() {
  const el = document.getElementById('typewriter-text');
  if (!el) return;

  const titles = [
    'Data Scientist',
    'Machine Learning Engineer',
    'Predictive Modeler',
    'Data Visualization Expert',
    'AI / ML Enthusiast',
  ];

  let titleIndex = 0;
  let charIndex  = 0;
  let isDeleting = false;

  function tick() {
    const current = titles[titleIndex];

    if (isDeleting) {
      charIndex--;
    } else {
      charIndex++;
    }

    el.textContent = current.slice(0, charIndex);

    let delay = isDeleting ? 55 : 100;

    if (!isDeleting && charIndex === current.length) {
      isDeleting = true;
      delay = 1800;
    } else if (isDeleting && charIndex === 0) {
      isDeleting = false;
      titleIndex = (titleIndex + 1) % titles.length;
      delay = 350;
    }

    setTimeout(tick, delay);
  }

  // Small initial delay so page has settled
  setTimeout(tick, 600);
}

/* ===================================================
   NAVBAR — SCROLL & ACTIVE LINK
=================================================== */
function initNavScroll() {
  const navbar   = document.getElementById('navbar');
  const navLinks = document.querySelectorAll('.nav-link[data-section]');
  const sections = document.querySelectorAll('section[id]');
  if (!navbar) return;

  function onScroll() {
    // Sticky blur
    navbar.classList.toggle('scrolled', window.scrollY > 50);

    // Active link: find which section's top is closest to nav-height
    let current = '';
    const offset = navbar.offsetHeight + 10;

    sections.forEach(section => {
      if (section.getBoundingClientRect().top <= offset) {
        current = section.id;
      }
    });

    navLinks.forEach(link => {
      link.classList.toggle('active', link.dataset.section === current);
    });
  }

  window.addEventListener('scroll', onScroll, { passive: true });
  onScroll(); // run once on load
}

/* ===================================================
   HAMBURGER MENU
=================================================== */
function initHamburger() {
  const hamburger  = document.getElementById('hamburger');
  const mobileNav  = document.getElementById('mobileNav');
  const mobileLinks = document.querySelectorAll('.mobile-nav-link');
  if (!hamburger || !mobileNav) return;

  function close() {
    hamburger.classList.remove('open');
    mobileNav.classList.remove('open');
    hamburger.setAttribute('aria-expanded', 'false');
  }

  hamburger.addEventListener('click', () => {
    const isOpen = mobileNav.classList.toggle('open');
    hamburger.classList.toggle('open', isOpen);
    hamburger.setAttribute('aria-expanded', String(isOpen));
  });

  // Close on link click
  mobileLinks.forEach(link => link.addEventListener('click', close));

  // Close on outside click
  document.addEventListener('click', e => {
    if (!hamburger.contains(e.target) && !mobileNav.contains(e.target)) {
      close();
    }
  });
}

/* ===================================================
   SCROLL REVEAL
=================================================== */
function initRevealOnScroll() {
  const revealEls = document.querySelectorAll('.reveal');
  if (!revealEls.length) return;

  const observer = new IntersectionObserver(entries => {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        entry.target.classList.add('visible');
        observer.unobserve(entry.target);
      }
    });
  }, { threshold: 0.12, rootMargin: '0px 0px -40px 0px' });

  revealEls.forEach((el, i) => {
    // Stagger siblings automatically based on their order among peers
    const siblings = el.parentElement.querySelectorAll('.reveal');
    let siblingIndex = 0;
    siblings.forEach((s, idx) => { if (s === el) siblingIndex = idx; });
    el.style.transitionDelay = `${siblingIndex * 0.07}s`;
    observer.observe(el);
  });
}

/* ===================================================
   SKILL BARS
=================================================== */
function initSkillBars() {
  const fills = document.querySelectorAll('.skill-fill');
  if (!fills.length) return;

  const observer = new IntersectionObserver(entries => {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        const fill = entry.target;
        fill.style.width = (fill.dataset.width || 0) + '%';
        observer.unobserve(fill);
      }
    });
  }, { threshold: 0.5 });

  fills.forEach(fill => observer.observe(fill));
}

/* ===================================================
   3D TILT EFFECT
=================================================== */
function initTiltEffect() {
  const cards = document.querySelectorAll('.tilt-card');
  if (!cards.length) return;

  // Skip tilt on touch devices (no mousemove)
  if (window.matchMedia('(hover: none)').matches) return;

  const MAX_TILT = 10; // degrees

  cards.forEach(card => {
    const inner = card.querySelector('.project-card-inner');
    if (!inner) return;

    card.addEventListener('mousemove', e => {
      const rect = card.getBoundingClientRect();
      const x = (e.clientX - rect.left) / rect.width  - 0.5; // -0.5 → 0.5
      const y = (e.clientY - rect.top)  / rect.height - 0.5;

      const rotateX = -y * MAX_TILT;
      const rotateY =  x * MAX_TILT;

      inner.style.transform =
        `perspective(1000px) rotateX(${rotateX}deg) rotateY(${rotateY}deg) scale(1.02)`;
    });

    card.addEventListener('mouseleave', () => {
      inner.style.transform =
        'perspective(1000px) rotateX(0deg) rotateY(0deg) scale(1)';
    });
  });
}

/* ===================================================
   CONTACT FORM
=================================================== */
function initContactForm() {
  const form       = document.getElementById('contactForm');
  const submitBtn  = document.getElementById('submitBtn');
  const statusEl   = document.getElementById('formStatus');
  if (!form) return;

  function isValidEmail(email) {
    return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);
  }

  function showStatus(msg, type) {
    statusEl.textContent = msg;
    statusEl.className   = 'form-status ' + type;
  }

  function clearStatus() {
    statusEl.textContent = '';
    statusEl.className   = 'form-status';
  }

  form.addEventListener('submit', e => {
    e.preventDefault();
    clearStatus();

    const name    = form.name.value.trim();
    const email   = form.email.value.trim();
    const subject = form.subject ? form.subject.value.trim() : 'No subject';
    const message = form.message.value.trim();

    if (!name || !email || !message) {
      showStatus('Please fill in all required fields.', 'error');
      return;
    }

    if (!isValidEmail(email)) {
      showStatus('Please enter a valid email address.', 'error');
      return;
    }

    // Simulate send — swap this block for Formspree/EmailJS/Netlify Forms later
    submitBtn.innerHTML = '<i class="fa-solid fa-spinner fa-spin"></i> Sending…';
    submitBtn.disabled  = true;

    setTimeout(() => {
      showStatus('Message sent! I\'ll get back to you soon. 🚀', 'success');
      form.reset();
      submitBtn.innerHTML = '<i class="fa-solid fa-paper-plane"></i> Send Message';
      submitBtn.disabled  = false;
    }, 1200);
  });

  // Clear error status as user types
  form.querySelectorAll('input, textarea').forEach(field => {
    field.addEventListener('input', clearStatus);
  });
}

/* ===================================================
   DATA VISUALIZATION — RADAR CHART
=================================================== */
function initDataViz() {
  const canvas = document.getElementById('skillsRadarChart');
  if (!canvas || typeof Chart === 'undefined') return;

  const observer = new IntersectionObserver(entries => {
    if (!entries[0].isIntersecting) return;
    observer.unobserve(canvas);

    new Chart(canvas, {
      type: 'radar',
      data: {
        labels: ['Python', 'SQL', 'Machine\nLearning', 'Statistical\nModeling', 'Data\nEngineering', 'Cloud\nPlatforms', 'Data\nVisualization', 'Big Data'],
        datasets: [{
          label: 'Proficiency',
          data: [93, 92, 88, 90, 80, 75, 85, 78],
          backgroundColor: 'rgba(0, 245, 255, 0.08)',
          borderColor: '#00f5ff',
          borderWidth: 2,
          pointBackgroundColor: '#00f5ff',
          pointBorderColor: 'transparent',
          pointHoverBackgroundColor: '#7b2fff',
          pointHoverBorderColor: '#7b2fff',
          pointRadius: 5,
          pointHoverRadius: 7,
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: true,
        animation: { duration: 1200, easing: 'easeInOutQuart' },
        layout: {
          padding: { top: 24, bottom: 24, left: 24, right: 24 }
        },
        scales: {
          r: {
            min: 0,
            max: 100,
            ticks: { display: false, stepSize: 25 },
            grid: { color: 'rgba(0, 245, 255, 0.08)' },
            angleLines: { color: 'rgba(0, 245, 255, 0.1)' },
            pointLabels: {
              color: '#a0a0c0',
              font: { family: "'Fira Code', monospace", size: 12, weight: '500' },
              padding: 16,
            }
          }
        },
        plugins: {
          legend: { display: false },
          tooltip: {
            backgroundColor: '#12121f',
            borderColor: 'rgba(0, 245, 255, 0.2)',
            borderWidth: 1,
            titleColor: '#00f5ff',
            bodyColor: '#e0e0ff',
            callbacks: {
              title: ctx => ctx[0].label.replace('\n', ' '),
              label: ctx => `  Proficiency: ${ctx.raw}%`
            }
          }
        }
      }
    });
  }, { threshold: 0.25 });

  observer.observe(canvas);
}

/* ===================================================
   LOADING SCREEN
=================================================== */
function initLoader() {
  const loader = document.getElementById('loader');
  if (!loader) return;

  const MIN_MS = 700;
  const start  = Date.now();

  function hide() {
    const wait = Math.max(0, MIN_MS - (Date.now() - start));
    setTimeout(() => {
      loader.classList.add('hidden');
      loader.addEventListener('transitionend', () => loader.remove(), { once: true });
    }, wait);
  }

  if (document.fonts && document.fonts.ready) {
    document.fonts.ready.then(hide);
  } else {
    hide();
  }
}

/* ===================================================
   SCROLL PROGRESS BAR
=================================================== */
function initScrollProgress() {
  const bar = document.getElementById('scrollProgress');
  if (!bar) return;

  function update() {
    const scrollable = document.documentElement.scrollHeight - window.innerHeight;
    bar.style.width = (scrollable > 0 ? (window.scrollY / scrollable) * 100 : 0) + '%';
  }

  window.addEventListener('scroll', update, { passive: true });
  update();
}

/* ===================================================
   CERTIFICATE MODAL
=================================================== */
function initCertModal() {
  const modal    = document.getElementById('certModal');
  if (!modal) return;

  const backdrop = modal.querySelector('.cert-modal-backdrop');
  const closeBtn = document.getElementById('certModalClose');
  const frame    = document.getElementById('certModalFrame');
  const titleEl  = document.getElementById('certModalTitle');
  const linkEl   = document.getElementById('certModalLink');

  function open(href, title) {
    titleEl.textContent = title;
    linkEl.href         = href;
    frame.src           = href;
    modal.hidden        = false;
    document.body.style.overflow = 'hidden';
    closeBtn.focus();
  }

  function close() {
    modal.hidden = true;
    document.body.style.overflow = '';
    frame.src = ''; // stop loading PDF when closed
  }

  // Use event delegation; PDF path stored in data-pdf so browser never intercepts it
  document.addEventListener('click', e => {
    const btn = e.target.closest('.cert-btn');
    if (!btn) return;
    const pdf = btn.getAttribute('data-pdf');
    if (!pdf) return; // no data-pdf → external link, let it open normally
    e.preventDefault();
    e.stopPropagation();
    const card  = btn.closest('.cert-card');
    const title = card ? card.querySelector('.cert-title').textContent.trim() : 'Certificate';
    open(pdf, title);
  });

  backdrop.addEventListener('click', close);
  closeBtn.addEventListener('click', close);
  document.addEventListener('keydown', e => {
    if (e.key === 'Escape' && !modal.hidden) close();
  });
}

/* ===================================================
   FOOTER — DYNAMIC YEAR
=================================================== */
function initFooterYear() {
  const yearEl = document.getElementById('year');
  if (yearEl) yearEl.textContent = new Date().getFullYear();
}

/* ===================================================
   ENTRY POINT
=================================================== */
document.addEventListener('DOMContentLoaded', () => {
  initLoader();
  initScrollProgress();
  initParticles();
  initTypewriter();
  initNavScroll();
  initHamburger();
  initRevealOnScroll();
  initSkillBars();
  initTiltEffect();
  initDataViz();
  initContactForm();
  initCertModal();
  initFooterYear();
});
