function showView(viewId) {
        $('.auth-view').hide();
        $('#' + viewId).show();
     }

function deleteKeyword(groupId, keywordIndex) {
        Shiny.setInputValue('delete_individual_keyword', {
          group_id: groupId,
          keyword_index: keywordIndex,
          timestamp: new Date().getTime()
        }, {priority: 'event'});
      }

      // Handle add keyword button clicks from suggestions
      $(document).on('click', '.add-keyword-btn', function() {
        var keyword = $(this).data('keyword');
        Shiny.setInputValue('add_suggested_keyword', keyword, {priority: 'event'});
      });
