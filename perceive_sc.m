function [i, o] = perceive_sc(vec, pts)
    % Check if vec is not a row vector
    if size(vec, 1) ~= 1
        vec = vec';
    end

    % loop through each element in pts
    for a = 1:length(pts)
        % compute the index of the closest value in vec to pts(a)
        [~, i(a)] = min( abs( diff( [ vec; ones(size(vec)) .* pts(a)]) ) );
        % Store the closest value from vec in o(a)
        o(a) = vec(i(a));
    end
end
    