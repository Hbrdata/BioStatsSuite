$(document).ready(function () {
  var dragSrcEl = null;
  var lastOverEl = null;

  $(document).on('dragstart', '.rcfg-sortable-row', function (e) {
    dragSrcEl = this;
    this.classList.add('rcfg-dragging');
    e.originalEvent.dataTransfer.effectAllowed = 'move';
    e.originalEvent.dataTransfer.setData('text/plain', '');
  });

  $(document).on('dragend', '.rcfg-sortable-row', function () {
    this.classList.remove('rcfg-dragging');
    $('.rcfg-sortable-row').removeClass('rcfg-drag-over-top rcfg-drag-over-bottom');
    lastOverEl = null;
  });

  $(document).on('dragover', '.rcfg-sortable-row', function (e) {
    e.preventDefault();
    e.originalEvent.dataTransfer.dropEffect = 'move';
    if (this === dragSrcEl) return false;

    // Only update indicator when hovering a different row
    if (this !== lastOverEl) {
      if (lastOverEl) {
        lastOverEl.classList.remove('rcfg-drag-over-top', 'rcfg-drag-over-bottom');
      }
      lastOverEl = this;
    }

    var rect = this.getBoundingClientRect();
    var midY = rect.top + rect.height / 2;
    if (e.originalEvent.clientY < midY) {
      this.classList.add('rcfg-drag-over-top');
      this.classList.remove('rcfg-drag-over-bottom');
    } else {
      this.classList.add('rcfg-drag-over-bottom');
      this.classList.remove('rcfg-drag-over-top');
    }
    return false;
  });

  $(document).on('dragleave', '.rcfg-sortable-row', function (e) {
    // Only clear if leaving to outside the row (not between children)
    var related = e.relatedTarget;
    if (related && this.contains(related)) return;
    this.classList.remove('rcfg-drag-over-top', 'rcfg-drag-over-bottom');
    if (lastOverEl === this) lastOverEl = null;
  });

  $(document).on('drop', '.rcfg-sortable-row', function (e) {
    e.preventDefault();
    e.stopPropagation();

    if (dragSrcEl === this) return;

    var $container = $(this).closest('.rcfg-sortable');
    if (!$container.length) return;

    var rect = this.getBoundingClientRect();
    var midY = rect.top + rect.height / 2;

    if (e.originalEvent.clientY < midY) {
      $(this).before(dragSrcEl);
    } else {
      $(this).after(dragSrcEl);
    }

    $('.rcfg-sortable-row').removeClass('rcfg-drag-over-top rcfg-drag-over-bottom');

    // Collect new order and send to Shiny
    var newOrder = [];
    $container.find('.rcfg-sortable-row').each(function () {
      var cid = $(this).data('cid');
      if (cid !== undefined) newOrder.push(Number(cid));
    });

    var ns = $container.data('ns');
    if (ns && newOrder.length > 0) {
      Shiny.setInputValue(ns + 'drag_reorder', newOrder, {priority: 'event'});
    }
  });
});
