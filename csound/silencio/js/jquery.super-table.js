/**
 * SuperTable makes the thead of any Table scroll with the browser
 * This is sometimes preferable over pure CSS table head scrolling as it
 * gives the user a more fluid experience if they prefer not to have more
 * than the one scroll-bar in the browser window.
 *
 * Author: Andrew Hackley
 * Released under the MIT license
 * https://github.com/hackley2/super-table
 *
 * Date: 2016-01-012
 */
(function ( $ ) {

$.fn.superTable = function(options){

	return this.each(function(){

		// Make the original table referenceable inside this class's methods
		var origTable = $(this);

		// Possible options that can be passed in as arguments
		var settings = $.fn.superTable.defaults;
		$.extend(settings,options);

		// Indicated whether the scrolling header and left column are currently
		// visible or hidden
		var cloneHeadShown = false;
		var cloneColumnShown = false;


        var tclass = '';
		// Check to see if this table is a superTable already
        if(origTable.attr("data-ST-id")){
            // If this table is already a superTable, then use it's unique class
            tclass = origTable.attr("data-ST-id");
        }else{
            // Set tclass so that the original table and cloned tables can be
            // referenced easily via a single class
            setUniqueTableClass();
        }


		// Scrolling table header attributes
		var scrollingHeadCloneID = getUniqueTableClass()+"_head";
		/**
		 * Strings that can be used in jQuery's select
		 * statement $().They should select the cloned table that
		 * is being used for the scrollable table head
		 */
		var scrollingHeadCloneSelect = "#"+scrollingHeadCloneID;
		var scrollingHeadCloneSelectDiv = scrollingHeadCloneSelect+"div";


		// Scrolling table column attributes
		var scrollingColumnCloneID = getUniqueTableClass()+"_column";
		/**
		 * Strings that can be used in jQuery's select
		 * statement $().They should select the cloned table that
		 * is being used for the scrollable table head
		 */
		var scrollingColumnCloneSelect = "#"+scrollingColumnCloneID;
		var scrollingColumnCloneSelectDiv = scrollingColumnCloneSelect+"div";


		// Now that variables are setup, start the core scrollTable function
		constructor(options);


		function constructor(options){
			// Load the user-defined settings
			$.extend(settings, options);

			// Enable scrollable table head rows and left column
			manageTableScrolling();

			// Enable collapsible rows
			if(settings.rowCollapse){
				collapsibleRows();
			}
			// Enable collapsible columns
			if(settings.columnCollapse){
				collapsibleColumns();
			}

			// If remove is set, then then the super table elements have already
			// been removed
			if(settings.remove == true){
				// Turn this setting back to false so that
				// scrolling can be enabled next time
				settings.remove = false;

				// Return. we're done.
				return;
			}
			// Start the table fully collapsed?
			if(settings.startCollapsed){
				manageCollapsingRows(false);
				manageCollapsingColumns(false);
			}
		}

		/**
		 * Super Table starter function.
		 * Initializes the scrollable column and header if needed
		 *
		 */
		function manageTableScrolling(){
			if(settings.scrollHead){
				scrollTableHead();
			}
			if(settings.scrollColumn){
				scrollTableLColumn();
			}
		}

		/**
		 * Called once to initialize the scrolling table head functionality
		 * on the given table
		 *
		 */
		function scrollTableHead(){

			if(settings.remove){
				//remove scrolling table head and return
				$(scrollingHeadCloneSelectDiv).remove();
                $(window).off("scroll");
                $(window).off("resize");
				return;
			}

			// insert the cloned header into the DOM
			// or update the table header if the header already exists
			if($(scrollingHeadCloneSelect).length != 0){
				resetClonedHeaderContainerWidth();
				moveCloneHead();
			}else{

				initializeTableHeadScrolling();
			}
		}


		function initializeTableHeadScrolling(){

			var cloneClasses = origTable.attr("class");
			var fixedHead =
				"<div id='"+scrollingHeadCloneID+"div' " +
					 "class='hidden' " +
					 "style='display:none; " +
					 "overflow:hidden; " +
					 "border-bottom:1px solid "+origTable.css("border-right-color")+";'>"+
					"<table id='"+scrollingHeadCloneID+"' " +
						   "class='"+cloneClasses+"' " +
						   "style='border: none !important;'>" +
					"</table>" +
				"</div>";


			$("body").append(fixedHead);

			// Copy the left and right border attributes of the primary table to
			//      the cloned head container
			// This ensures the table inside the container has a correct width
			// The border is applied to the containing div instead of the table in
			//     due to some browser incompatibilities when tables are re-sized
			$(scrollingHeadCloneSelectDiv).css("border-left-width", origTable.css("border-left-width"));
			$(scrollingHeadCloneSelectDiv).css("border-left-color", origTable.css("border-left-color"));
			$(scrollingHeadCloneSelectDiv).css("border-left-style", origTable.css("border-left-style"));
			$(scrollingHeadCloneSelectDiv).css("border-right-width",origTable.css("border-right-width"));
			$(scrollingHeadCloneSelectDiv).css("border-right-color",origTable.css("border-right-color"));
			$(scrollingHeadCloneSelectDiv).css("border-right-style",origTable.css("border-right-style"));

			// Clone the header rows from the original table
			var clone = origTable.children("thead").clone();
			clone.appendTo(scrollingHeadCloneSelect);

			// Insert the cloned header rows into the DOM
			clone = origTable.children("tbody").clone();
			clone.appendTo(scrollingHeadCloneSelect);
			// Initialize the cloned header to be fixed at the top of the page
			var pos = origTable.offset();

			// Initial CSS for cloned header's containing div
			$(scrollingHeadCloneSelectDiv).css(
				{
					position: "fixed",
					marginLeft: "0px",
					marginTop: "0px",
					top:"0px",
					left:pos.left,
					"z-index": "1111"
				}
			);

			// Set cloned header's containing div's height & width
			resetClonedHeaderContainerHeight();
			resetClonedHeaderContainerWidth();

			// When the page is loaded for the first time
			// reposition/hide/show the header if needed
			moveCloneHead();

			// When the page scrolls
			$(window).on("scroll",function(){
				//reposition/hide/show the header if needed
				moveCloneHead();
			});

			// When the page is resized
			$(window).on("resize",function(){

				// Set cloned header's containing div's height & width
				resetClonedHeaderContainerWidth();
				resetClonedHeaderContainerHeight();
				// Reposition/hide/show the header if needed
				moveCloneHead();
			});
		}

		/**
		 * Show the cloned header
		 */
		function showCloneHead() {
			if(!cloneHeadShown){
				$(scrollingHeadCloneSelectDiv).show().css("display","block");
				cloneHeadShown = true;
			}
		}

		/**
		 * Show the cloned left column
		 */
		function showCloneColumn() {
			if(!cloneColumnShown) {
				$(scrollingColumnCloneSelectDiv).show().css("display", "block");
				cloneColumnShown = true;
			}
		}

		/**
		 * Hide the cloned header
		 */
		function hideCloneHead() {
			if(cloneHeadShown) {
				$(scrollingHeadCloneSelectDiv).hide();
				cloneHeadShown = false;
			}
		}

		/**
		 * Hide the cloned left column
		 */
		function hideCloneColumn() {
			if(cloneColumnShown) {
				$(scrollingColumnCloneSelectDiv).hide();
				cloneColumnShown = false;
			}
		}

		/**
		 * Set the cloned header's containing div's height to equal the height of
		 * the thead element of the original table
		 */
		function resetClonedHeaderContainerHeight(){
			$(scrollingHeadCloneSelectDiv).css("height",origTable.children("thead").height());
		}
		/**
		 * Set the cloned header's containing div's width to equal the width of
		 * the original table
		 */
		function resetClonedHeaderContainerWidth(){
			$(scrollingHeadCloneSelectDiv).css('width',origTable.outerWidth());
		}

		/**
		 * Set the scrolling column's containing div's width to equal the width of
		 * the original table. Also resets the scrolling column's table's width.
		 */
		function resetClonedColumnContainerWidth(){
			$(scrollingColumnCloneSelectDiv).css('width',findMaxWidthOfTableCell());
			$(scrollingColumnCloneSelect).css('width',origTable.outerWidth());
		}

		/**
		 * Move the cloned table head so that it stays at the top of the browser
		 * window on top of the corresponding table if the table is visible
		 */
		function moveCloneHead(){
			// If the original table is currently visible
			if(origTable.filter(":visible").length >= 1){
				// Document all the offsets and heights of the cloned header
				// and the original table
				var origPos = origTable.offset();
				var origHeight =  origTable.height();
				var posFixed = $(scrollingHeadCloneSelectDiv).offset();
				var fixedHeadHeight = $(scrollingHeadCloneSelectDiv).height();
				var windowLeft = $(window).scrollLeft();
				var windowTop = $(window).scrollTop();

				// Make sure the left offset of the clone matches the left
				// offset of the original table
				if(origPos.left != windowLeft + posFixed.left){
					var leftDiff = 0 -(windowLeft - origPos.left);
					$(scrollingHeadCloneSelectDiv).css("left", leftDiff+"px");
				}

				// If the original table's header rows are above the browser
				// window, but the bottom of the table is still viewable in
				// the browser window
				if( origPos.top < posFixed.top
				 && origPos.top + origHeight > posFixed.top
				) {
					// Diff < 0 when the bottom of the original table is above the
					// bottom of the cloned header
					var diff = origPos.top + origHeight - windowTop;
					diff = diff - fixedHeadHeight;
					if(diff <= 0){
						// Show the cloned header slightly above the browser window
						$(scrollingHeadCloneSelectDiv).css("top", diff+"px");
					}else{
						// Show the cloned header at the top of the browser window
						$(scrollingHeadCloneSelectDiv).css("top", "0px");
					}
					showCloneHead();
				}else{
					// hide the cloned header
					hideCloneHead();
				}
			}else{
				// hide the cloned header
				hideCloneHead();
			}
		}

		/**
		 * Called once to initialize scrolling ability of the left most column on
		 * the given table. The left most column will then scroll with the browser
		 * window so that it will always be visible, if the given table is visible.
		 *
		 */
		function scrollTableLColumn(){

			if(settings.remove){
				// Remove the scrolling left column and return
				$(scrollingColumnCloneSelectDiv).remove();
                $(window).off("scroll");
                $(window).off("resize");
				return;
			}

			var cloneClasses = origTable.attr("class");
			var twidth = origTable.outerWidth();

			var wide = findMaxWidthOfTableCell();

			// Update the table header if the header already exists
			if($(scrollingColumnCloneSelect).length != 0){
				$(scrollingColumnCloneSelectDiv).width(wide);
			}
			// Insert the cloned header into the DOM
			else{

				var fixedCol =
					"<div id='"+scrollingColumnCloneID+"div' " +
						 "style='display:none; " +
						 "overflow:hidden; " +
						 "border-right:1px solid; " +
						 "background:white; "+origTable.css("border-right-color")+";'>" +
						"<table id='"+scrollingColumnCloneID+"' " +
							   "class='"+cloneClasses+"' " +
							   "style='padding:0; " +
									  "background: white; " +
									  "width:"+twidth+"px; " +
									  "border-bottom-width:"+origTable.css("border-bottom-width")+"; " +
									  "top: -1px;'>" +
						"</table>" +
					"</div>";

				// Insert the cloned header into the DOM
				$("body").append(fixedCol);

				// Clone the header column from the original table
				var orig = origTable.children("thead");
				var clone = orig.clone();
				//insert the cloned header rows into the DOM
				clone.appendTo(scrollingColumnCloneSelect);

				// Clone the body from the original table
				orig = origTable.children("tbody");
				clone = orig.clone();
				// Insert the cloned header rows into the DOM
				clone.appendTo(scrollingColumnCloneSelect);


				$(scrollingColumnCloneSelectDiv).width(wide);

				// Initialize the cloned left column to be fixed at the left side
				// of the page
				$(scrollingColumnCloneSelectDiv).css(
					{
						position: "fixed",
						marginLeft: "0px",
						marginTop: "0px",
						top:"0px",
						left:"0px",
						"z-index": "1110"
					}
				);
				$(scrollingColumnCloneSelect).css(
					{
						"background-color": origTable.find(" th:nth-child(2)").css("background-color"),
						marginBottom: "0px"
					}
				);

				// When the page is loaded for the first time
				// reposition/hide/show the header if needed
				moveCloneLColumn();

				// When the page scrolls
				$(window).on("scroll",function(){
					// Reposition/hide/show the header if needed
					moveCloneLColumn();
				});

				//When the page is resized
				$(window).on("resize",function(){
					// Reposition/hide/show the header if needed
					moveCloneLColumn();
				});
			}
		}


		/**
		 * called every time a table's scrollable column needs to move.  this is typically done every
		 * time the browser is resized or scrolled
		 *
		 */
		function moveCloneLColumn(){
			if(origTable.filter(":visible").length >= 1){
				// Document all the offsets and heights of the cloned left column and the original table
				var origPos = origTable.offset();
				var origWidth =  origTable.width();
				var clonePos = $(scrollingColumnCloneSelectDiv).offset();
				var fixedColWidth = $(scrollingColumnCloneSelectDiv).width();
				var windowLeft = $(window).scrollLeft();
				var windowTop = $(window).scrollTop();
				var windowWidth = $(window).width();

				// Make sure the clone container is showing the right amount of table
				resetClonedColumnContainerWidth();

				// Make sure the top offset of the clone matches the top offset
				// of the original table
				if(origPos.top != clonePos.top){
					var topDiff = 0 -(windowTop - origPos.top);

					$(scrollingColumnCloneSelectDiv).css("top", topDiff+"px");
				}

				// If the original table's header rows are left of the browser
				// window, but the bottom or top of the table is still viewable
				// in the browser window
				if(origPos.left < windowLeft && origPos.left < windowLeft + windowWidth){
					// Diff < 0 when the right side of the original table is left
					// the right side of the cloned column
					var diff = origPos.left + origWidth - clonePos.left - fixedColWidth;

					if(diff < 0){
						// Show the cloned column slightly left of the browser window
						$(scrollingColumnCloneSelectDiv).css("left", diff+"px");
						showCloneColumn();
					}else{
						// Show the cloned column at the left of the browser window
						$(scrollingColumnCloneSelectDiv).css("left", "0px");
						showCloneColumn();
					}

				}else{
					// Hide the cloned header
					hideCloneColumn();
				}
			}else{
				// Hide the cloned header
				hideCloneColumn();
			}
		}


		/**
		 * Initializes a table's collapsible column functionality.
		 * If any column has a data-ST-group
		 * attribute, then this column will only expand or collapse
		 * other columns that share this attribute and
		 * attribute value
		 *
		 *
		 */
		function collapsibleColumns(){

			var $collapsingColumnInteractionHeader = $("."+getUniqueTableClass()+">thead>tr>th");
            var $expandCollapseAllElemnt = $('*[data-super-table="'+origTable.attr('id')+'"]');

            if(settings.remove){
                $collapsingColumnInteractionHeader.off("click");
                $expandCollapseAllElemnt.off("click");
                $collapsingColumnInteractionHeader.css('cursor','auto');
                return;
            }

			// Give the table's thead cells the cursor:pointer css styling so
			// that the user can easily tell that something happens when they click on it
			$collapsingColumnInteractionHeader.css('cursor','pointer');

			$collapsingColumnInteractionHeader.on("click",function(){
				var groupID = $(this).attr('data-ST-group');
				manageCollapsingColumns(groupID);
			});

			// If it exists, enable a 'collapse/expand all' element
			$expandCollapseAllElemnt.on("click",function(){
				var groupID = false;
				manageCollapsingColumns(groupID);
			});

		}

		/**
		 * Called when columns needs to be expanded or collapsed
		 *
		 * @param {Object} groupID
		 */
		function manageCollapsingColumns(groupID){
			var groupAttr = '';
			if(groupID){
				groupAttr = "[data-ST-group='"+groupID+"']";
			}

			// When columns are expanded or collapsed, there may be a class
			// that should be applied to each state (could be used for CSS purposes/etc.)
			var swapClasses = false;
			if( settings.columnCollapsedClass != settings.columnExpandedClass
		     && settings.columnCollapsedClass != ''
			 && settings.columnExpandedClass != ''
			) {
				swapClasses = true;
			}

			// The columns are showing and need to be hidden
			var $columnGroup = $("."+getUniqueTableClass()+" .columnHideable"+groupAttr);
			if($columnGroup.css('display') != 'none'){
				// Hide the columns
				$columnGroup.hide();

				// Adjust colspans as necessary for each cell in each table
				$("."+getUniqueTableClass()+" "+groupAttr+"[data-colspanmin]").each(function(){
					var colMin = $(this).attr("data-colspanmin");
					$(this).attr('colspan',colMin);
				});

				if(swapClasses){
					// Swap classes
					$("."+getUniqueTableClass()+" ."+settings.columnExpandedClass+groupAttr).removeClass(settings.columnExpandedClass).addClass(settings.columnCollapsedClass);
				}
			}
			// The columns are hidden and should be shown
			else{
				// Show the columns
				$columnGroup.show();

				// Adjust colspans as necesary for each cell in each table
				$("."+getUniqueTableClass()+" "+groupAttr+"[data-colspanmax]").each(function(){
					var colMax = $(this).attr("data-colspanmax");
					$(this).attr('colspan',colMax);
				});

				if(swapClasses){
					// Swap classes
					$("."+getUniqueTableClass()+" ."+settings.columnCollapsedClass+groupAttr).removeClass(settings.columnCollapsedClass).addClass(settings.columnExpandedClass);
				}
			}

			// Set cloned header's containing div's height
			resetClonedHeaderContainerHeight();
			// Once in a while expanding columns will change row widths and mess up
			// the scrolling column. So, sync them now.
			resetClonedColumnContainerWidth();

			manageTableScrolling();
		}

		/**
		 * Called to initialize a table's collapsable row functionality.  if any row has a data-ST-group
		 * attirbute, then this row will only expand or collapse other rows that share this attribute and
		 * attribute value
		 *
		 *
		 */
		function collapsibleRows(){

			// Give the first column of each row the cursor:pointer css styling so
			// that the user can easily tell that something happens when they click on it
			var $collapsingRowInteractionColumn = $("."+getUniqueTableClass()+">tbody>tr>td:first-child");
			var $collapsingHeadRowInteractionColumn = $("."+getUniqueTableClass()+">thead>tr>th:first-child");
            var $expandCollapseAllElement = $('*[data-super-table="'+origTable.attr('id')+'"]');

            if(settings.remove){
                $collapsingRowInteractionColumn.off("click");
                $collapsingHeadRowInteractionColumn.off("click");
                $expandCollapseAllElement.off("click");
                $collapsingRowInteractionColumn.css('cursor','auto');
                return;
            }

            $collapsingRowInteractionColumn.css('cursor','pointer');

			// Expand/collapse the appropriate ST-group of rows
			$collapsingRowInteractionColumn.on("click",function(){
				var groupID = $(this).parent().attr('data-ST-group');
				manageCollapsingRows(groupID);
			});
			// Expand/collapse all rows when the first cell of a header row is clicked
			$collapsingHeadRowInteractionColumn.on("click",function(){
				var groupID = false;
				manageCollapsingRows(groupID);
			});

			// If it exists, enable a 'collapse/expand all' element
			$expandCollapseAllElement.on("click",function(){
				var groupID = false;
				manageCollapsingRows(groupID);
			});
		}

		/**
		 * Called when a row or group of rows needs to be expanded or collapsed
		 *
		 * @param {Object} groupID
		 */
		function manageCollapsingRows(groupID){

			// When rows are expanded or collapsed, there may be a class
			// that should be applied to each state (used for CSS purposes/etc.)
			var swapClasses = false;
			if( settings.rowCollapsedClass != settings.rowExpandedClass
			 && settings.rowCollapsedClass != ''
			 && settings.rowExpandedClass != ''
			) {
				swapClasses = true;
			}


			// A table can have multiple groups of rows that expand and collapse
			// independently. if this is the case, then expand/collapse
            // the appropriate group of rows
			var groupSelector = "";
			if(groupID) {
				groupSelector = "[data-ST-group='" + groupID + "']";
			}

			// Rows to show or hide
			var rowsToShowOrHide = $("."+getUniqueTableClass()+" .rowHideable"+groupSelector);

			// Show the row group's hidden rows
			if(rowsToShowOrHide.hasClass("hidden")){
				rowsToShowOrHide.removeClass("hidden").show();
				// Swap row classes
				if(swapClasses){
					applyRowExpandedClass(groupSelector);
				}
			}else {
				// Hide the row group's hide-able rows
				rowsToShowOrHide.addClass("hidden").hide();
				// Swap row classes
				if (swapClasses) {
					applyRowCollapsedClass(groupSelector);
				}
			}
			// Make sure the scrolling column and thead still line up ok
			manageTableScrolling();
		}

		/**
		 * Swaps the expanded class with the collapsed class for the rows that
		 * are being collapsed
		 *
		 * @param groupSelector The string needed to select a group of rows that
		 *                      should be expanded and collapsed together and
		 *                      so together need their classes updated
		 */
		function applyRowCollapsedClass(groupSelector) {
			$("."+getUniqueTableClass()+" "+groupSelector+" ."+settings.rowCollapsedClass)
				.removeClass(settings.rowCollapsedClass)
				.addClass(settings.rowExpandedClass);
		}
		/**
		 * Swaps the collapsed class with the expanded class for the rows that
		 * are being expanded
		 *
		 * @param groupSelector The string needed to select a group of rows that
		 *                      should be expanded and collapsed together and
		 *                      so together need their classes updated
		 */
		function applyRowExpandedClass(groupSelector) {
			$("."+getUniqueTableClass()+" "+groupSelector+" ."+settings.rowExpandedClass)
				.removeClass(settings.rowExpandedClass)
				.addClass(settings.rowCollapsedClass);
		}

		/**
		 * Returns a class that is unique to the original table and clone tables
		 *
		 * @returns {string}
		 */
		function getUniqueTableClass(){
			return tclass;
		}

		/**
		 * Initializes the unique table class if it hasn't already been initialized
		 *
		 * @returns {string}
		 */
		function setUniqueTableClass(){
			if(tclass == '') {
                var uniqueClass = '';
				if (origTable.attr("id") == null || origTable.attr("id") == '') {
					for (var i = 0; tclass = ''; i++) {
						if ($('.ST_' + i).length == 0) {
							uniqueClass = 'ST_' + i;
						}
					}
				} else {
					uniqueClass = 'ST_' + origTable.attr('id');
				}
                setTclass(uniqueClass);
				// Add the unique class to the original table
				origTable.addClass(uniqueClass);
				// Add another class so that we can track which are superTables
				origTable.attr("data-ST-id",uniqueClass);
			}
		}

        function setTclass(uniqueTableClass){
            tclass = uniqueTableClass;
        }

		function findMaxWidthOfTableCell(){

			var wideBodyCell = findMaxWidthOfTableBodyCell();
			var wideHeaderCell = findMaxWidthOfTableHeadCell();
			return Math.max(wideBodyCell, wideHeaderCell);
		}

		/**
		 * Return the max width of a cell in he first column in the body of
		 * the original table
		 *
		 * @returns {number}
		 */
		function findMaxWidthOfTableBodyCell(){
			var wide = 0;
			// Some cells may be hidden and thus the width isn't necessarily
			// accurate. Loop through all cells in the first column for accuracy
			origTable.children("tbody").children("tr").children("td:first-child").each(function(){
				if(wide == 0){
					wide = $(this).innerWidth();
				}
			});
			return wide;
		}
		/**
		 * Return the max width of a cell in he first column in the header of
		 * the original table
		 *
		 * @returns {number}
		 */
		function findMaxWidthOfTableHeadCell(){
			var wide = 0;
			// Some cells may be hidden and thus the width isn't necessarily
			// accurate. Loop through all cells in the first column for accuracy
			origTable.children("thead").children("tr").children("th:first-child").each(function(){
				var wideTH = $(this).outerWidth();
				if(wide < wideTH){
					wide = wideTH;
				}
			});
			return wide;
		}

	});
}

// Making the default options accessible
$.fn.superTable.defaults = {
	scrollHead : true,
	scrollColumn : true,
	rowCollapse : false,
	rowCollapsedClass : '',
	rowExpandedClass : '',
	columnCollapse : false,
	columnCollapsedClass : '',
	columnExpandedClass : '',
	startCollapsed : false,
	remove : false
};

}( jQuery ));